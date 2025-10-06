use super::accumulator::BothAccumulators;
use super::params::*;
use crate::GetCheckedIfDebug;
use crate::chess::{chess_move::ChessMove, move_gen::MovesList, position::Position, types::*};
use arrayvec::ArrayVec;

pub type ScoredMoves = ArrayVec<(ChessMove, f32), 256>;

pub fn value_eval(both_accs: &mut BothAccumulators, stm: Color) -> i32 {
    let hl_activated: &[[i16; HL_SIZE / 2]; 2] = both_accs.activated_accs();

    let mut result: f32 = 0.0;

    for (is_nstm, &color) in [stm, !stm].iter().enumerate() {
        for i in 0..(HL_SIZE / 2) {
            unsafe {
                result += hl_activated[color][i] as f32 * NET.out_w_value.get_unchecked(is_nstm)[i];
            }
        }
    }

    result /= (FT_Q * FT_Q) as f32;
    result += NET.out_b_value;
    result *= VALUE_SCALE as f32;
    result.round() as i32
}

pub fn get_policy_logits<const Q_SEARCH: bool>(
    both_accs: &mut BothAccumulators,
    pos: &Position,
    legal_moves: &MovesList,
    exclude_move: Option<ChessMove>,
) -> ScoredMoves {
    let mut logits: ScoredMoves = ArrayVec::new_const();
    let king_sq: Square = pos.king_square(pos.side_to_move());

    for &mov in legal_moves {
        if Some(mov) == exclude_move {
            continue;
        }

        // In quiescence search, skip quiets, underpromotions and moves that lose material
        if Q_SEARCH && (pos.is_quiet_or_underpromotion(mov) || !pos.see_ge(mov, 0)) {
            continue;
        }

        let hl_activated: &[[i16; HL_SIZE / 2]; 2] = both_accs.activated_accs();
        let mut dst_for_logit_idx: Square = mov.dst();

        if pos.side_to_move() == Color::Black {
            dst_for_logit_idx = dst_for_logit_idx.rank_flipped();
        }

        if mov
            .promotion()
            .is_some_and(|promo_pt| promo_pt != PieceType::Queen)
        {
            dst_for_logit_idx = dst_for_logit_idx.rank_flipped();
        }

        if king_sq.file() < File::E {
            dst_for_logit_idx = dst_for_logit_idx.file_flipped();
        }

        let logit_idx: usize = (mov.piece_type() as usize) * 6 * 64
            + (dst_for_logit_idx as usize) * 6
            + (pos.piece_type_captured_by(mov).unwrap_or(PieceType::King) as usize);

        let mut logit: f32 = 0.0;

        unsafe {
            let out_w: &[[f32; HL_SIZE / 2]; 2] = NET.out_w_policy.get_checked_if_debug(logit_idx);

            for (is_nstm, &color) in [pos.side_to_move(), !pos.side_to_move()].iter().enumerate() {
                for i in 0..(HL_SIZE / 2) {
                    logit += hl_activated[color][i] as f32 * out_w.get_unchecked(is_nstm)[i];
                }
            }

            logit /= (FT_Q * FT_Q) as f32;
            logit += *NET.out_b_policy.get_checked_if_debug(logit_idx);

            logits.push_unchecked((mov, logit));
        }
    }

    logits
}

pub fn softmax(policy_logits: &mut ScoredMoves) {
    let mut sum_of_exps = 0.0;

    for (_, logit) in policy_logits.iter_mut() {
        *logit = logit.exp();
        sum_of_exps += *logit;
    }

    for (_, logit) in policy_logits.iter_mut() {
        *logit /= sum_of_exps;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess::{position::Position, util::FEN_START};
    use std::collections::HashMap;

    const FEN_START_FLIPPED: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1";

    const FEN_KIWIPETE_MODIFIED: &str =
        "r2k3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w K - 0 1";

    const FEN_KIWIPETE_MODIFIED_FLIPPED: &str =
        "r3k2r/pppbbppp/2n2q1P/1P2p3/3pn3/BN2PNP1/P1PPQPB1/R2K3R b k - 0 1";

    const FEN_MATE_IN_1_PROMOS_EP: &str = "3q4/4P3/8/5Pp1/8/2R5/2K5/k7 w - g6 0 1";
    const FEN_MATE_IN_1_PROMOS_EP_FLIPPED: &str = "K7/2k5/2r5/8/5pP1/8/4p3/3Q4 b - g3 0 1";

    const FEN_IN_CHECK: &str = "3b2k1/4P1p1/1K6/8/8/8/8/8 w - - 0 1";
    const FEN_IN_CHECK_FLIPPED: &str = "8/8/8/8/8/1k6/4p1P1/3B2K1 b - - 0 1";

    #[test]
    fn test_value() {
        let assert_value_eval = |fen: &str, expected_eval: i32| {
            let pos = Position::try_from(fen).unwrap();
            let mut both_accs = BothAccumulators::from(&pos);

            assert_eq!(
                value_eval(&mut both_accs, pos.side_to_move()),
                expected_eval
            );
        };

        for (fen, fen_flipped, expected_eval) in [
            (FEN_START, FEN_START_FLIPPED, 40),
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, -97),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                214,
            ),
            (FEN_IN_CHECK, FEN_IN_CHECK_FLIPPED, -258),
        ] {
            assert_value_eval(fen, expected_eval);
            assert_value_eval(fen_flipped, expected_eval);
        }
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_policy() {
        // These hashmaps map move to expected logit and expected softmax'd policy

        let start_pos_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("g1f3", (0.85, 0.22)),
            ("d2d4", (0.61, 0.17)),
            ("b1c3", (0.40, 0.14)),
            ("e2e4", (0.05, 0.10)),
            ("e2e3", (-0.31, 0.07)),
            ("d2d3", (-0.56, 0.05)),
            ("g2g3", (-0.70, 0.05)),
            ("c2c3", (-1.05, 0.03)),
            ("a2a3", (-1.05, 0.03)),
            ("c2c4", (-1.52, 0.02)),
            ("a2a4", (-1.59, 0.02)),
            ("b2b3", (-1.80, 0.02)),
            ("f2f4", (-1.97, 0.01)),
            ("h2h4", (-1.99, 0.01)),
            ("h2h3", (-2.02, 0.01)),
            ("b1a3", (-2.06, 0.01)),
            ("g1h3", (-2.17, 0.01)),
            ("b2b4", (-2.30, 0.01)),
            ("f2f3", (-2.54, 0.01)),
            ("g2g4", (-3.08, 0.00)),
        ]);

        let kiwipete_mod_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e2a6", (1.88, 0.35)),
            ("d5e6", (1.19, 0.18)),
            ("c3a4", (0.11, 0.06)),
            ("g2h3", (-0.03, 0.05)),
            ("c3d1", (-0.14, 0.05)),
            ("f3e3", (-0.41, 0.04)),
            ("g2g3", (-0.91, 0.02)),
            ("d2g5", (-0.93, 0.02)),
            ("g2g4", (-0.96, 0.02)),
            ("d5d6", (-1.35, 0.01)),
            ("e5d3", (-1.39, 0.01)),
            ("d2f4", (-1.50, 0.01)),
            ("f3f6", (-1.55, 0.01)),
            ("e1g1", (-1.56, 0.01)),
            ("c3b5", (-1.56, 0.01)),
            ("e2b5", (-1.60, 0.01)),
            ("d2e3", (-1.72, 0.01)),
            ("d2h6", (-1.74, 0.01)),
            ("f3f4", (-1.75, 0.01)),
            ("e5g6", (-1.81, 0.01)),
            ("f3h3", (-1.94, 0.01)),
            ("a1d1", (-1.98, 0.01)),
            ("c3b1", (-1.99, 0.01)),
            ("e5c6", (-1.99, 0.01)),
            ("e5g4", (-2.07, 0.01)),
            ("f3g3", (-2.08, 0.01)),
            ("e5f7", (-2.15, 0.01)),
            ("a1c1", (-2.20, 0.01)),
            ("e2f1", (-2.23, 0.01)),
            ("e2d3", (-2.53, 0.00)),
            ("a2a3", (-2.67, 0.00)),
            ("e5d7", (-2.86, 0.00)),
            ("b2b3", (-2.87, 0.00)),
            ("e1d1", (-2.94, 0.00)),
            ("d2c1", (-3.04, 0.00)),
            ("e5c4", (-3.08, 0.00)),
            ("f3d3", (-3.13, 0.00)),
            ("h1g1", (-3.24, 0.00)),
            ("a2a4", (-3.28, 0.00)),
            ("f3h5", (-3.30, 0.00)),
            ("f3g4", (-3.86, 0.00)),
            ("e2c4", (-3.95, 0.00)),
            ("e2d1", (-4.15, 0.00)),
            ("a1b1", (-4.26, 0.00)),
            ("e1f1", (-4.40, 0.00)),
            ("h1f1", (-4.46, 0.00)),
            ("f3f5", (-6.23, 0.00)),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("f5g6", (2.14, 0.17)),
            ("e7d8r", (2.14, 0.17)),
            ("e7d8b", (2.14, 0.17)),
            ("e7d8n", (2.14, 0.17)),
            ("e7d8q", (2.05, 0.15)),
            ("f5f6", (1.10, 0.06)),
            ("e7e8q", (0.67, 0.04)),
            ("c2b3", (-0.19, 0.02)),
            ("c2c1", (-0.40, 0.01)),
            ("c3c8", (-0.72, 0.01)),
            ("c3c6", (-0.74, 0.01)),
            ("c3d3", (-1.07, 0.01)),
            ("c3b3", (-1.44, 0.00)),
            ("c3c4", (-1.45, 0.00)),
            ("c3c7", (-1.68, 0.00)),
            ("c3e3", (-1.80, 0.00)),
            ("c3g3", (-2.20, 0.00)),
            ("c3f3", (-2.22, 0.00)),
            ("c3a3", (-2.24, 0.00)),
            ("c3c5", (-2.27, 0.00)),
            ("e7e8r", (-2.38, 0.00)),
            ("e7e8b", (-2.38, 0.00)),
            ("e7e8n", (-2.38, 0.00)),
            ("c3h3", (-3.05, 0.00)),
        ]);

        let in_check_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (0.18, 0.21)),
            ("e7d8r", (0.11, 0.19)),
            ("e7d8b", (0.11, 0.19)),
            ("e7d8n", (0.11, 0.19)),
            ("b6c6", (-1.15, 0.05)),
            ("b6c5", (-1.24, 0.05)),
            ("b6b5", (-1.74, 0.03)),
            ("b6a6", (-1.87, 0.03)),
            ("b6b7", (-1.97, 0.02)),
            ("b6a7", (-1.99, 0.02)),
        ]);

        let assert_policy = |fen: &str, expected: &HashMap<&str, (f32, f32)>| {
            let pos = Position::try_from(fen).unwrap();
            let mut both_accs = BothAccumulators::from(&pos);

            let logits: ScoredMoves =
                get_policy_logits::<false>(&mut both_accs, &pos, &pos.legal_moves(), None);

            assert_eq!(logits.len(), expected.len());

            let mut policy = logits.clone();
            softmax(&mut policy);

            for ((mov, logit), (_, move_policy)) in logits.iter().zip(&policy) {
                let mut move_oriented: ChessMove = *mov;

                // Flip move vertically if black to move
                if pos.side_to_move() == Color::Black {
                    let new_src: Square = mov.src().rank_flipped();
                    let new_dst: Square = mov.dst().rank_flipped();

                    if let Some(promo_pt) = mov.promotion() {
                        move_oriented = ChessMove::new_promotion(new_src, new_dst, promo_pt);
                    } else {
                        move_oriented = ChessMove::new(new_src, new_dst, mov.piece_type());
                    }
                }

                let (expected_logit, expected_move_policy) = *expected
                    .get(move_oriented.to_string().as_str())
                    .unwrap_or_else(|| panic!("Move {mov} not in expected"));

                assert!((logit - expected_logit).abs() < 0.005);
                assert!((move_policy - expected_move_policy).abs() < 0.005);
            }
        };

        for (fen, fen_flipped, expected_policy) in [
            (FEN_START, FEN_START_FLIPPED, &start_pos_policy),
            (
                FEN_KIWIPETE_MODIFIED,
                FEN_KIWIPETE_MODIFIED_FLIPPED,
                &kiwipete_mod_policy,
            ),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                &mate_in_1_promos_ep_policy,
            ),
            (FEN_IN_CHECK, FEN_IN_CHECK_FLIPPED, &in_check_policy),
        ] {
            assert_policy(fen, expected_policy);
            assert_policy(fen_flipped, expected_policy);
        }
    }
}
