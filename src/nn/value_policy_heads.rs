use super::accumulator::BothAccumulators;
use super::moves_map::get_move_idx_1882;
use super::params::*;
use arrayvec::ArrayVec;

use crate::chess::{
    chess_move::ChessMove,
    move_gen::MovesList,
    position::Position,
    types::{Color, File, Square},
};

pub type ScoredMoves = ArrayVec<(ChessMove, f32), 256>;

pub fn value_eval(both_accs: &mut BothAccumulators, stm: Color) -> i32 {
    let hl_activated: &[[i16; HALF_HL_SIZE]; 2] = both_accs.activated_accs();

    let mut result: f32 = 0.0;

    for (is_nstm, &color) in [stm, !stm].iter().enumerate() {
        for i in 0..HALF_HL_SIZE {
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

        let hl_activated: &[[i16; HALF_HL_SIZE]; 2] = both_accs.activated_accs();

        let mut new_src: Square = mov.src();
        let mut new_dst: Square = mov.dst();

        // Flip move vertically if black to move
        if pos.side_to_move() == Color::Black {
            new_src = new_src.rank_flipped();
            new_dst = new_dst.rank_flipped();
        }

        // Mirror move along vertical axis if stm king on left side of board
        if king_sq.file() < File::E {
            new_src = new_src.file_flipped();
            new_dst = new_dst.file_flipped();
        }

        let move_oriented: ChessMove = if let Some(promo_pt) = mov.promotion() {
            ChessMove::new_promotion(new_src, new_dst, promo_pt)
        } else {
            ChessMove::new(new_src, new_dst, mov.piece_type())
        };

        let move_idx_1882: usize = get_move_idx_1882(move_oriented);
        let mut logit: f32 = 0.0;

        unsafe {
            let out_w: &[[f32; HALF_HL_SIZE]; 2] = NET.out_w_policy.get_unchecked(move_idx_1882);

            for (is_nstm, &color) in [pos.side_to_move(), !pos.side_to_move()].iter().enumerate() {
                for i in 0..HALF_HL_SIZE {
                    logit += hl_activated[color][i] as f32 * out_w.get_unchecked(is_nstm)[i];
                }
            }

            logit /= (FT_Q * FT_Q) as f32;
            logit += *NET.out_b_policy.get_unchecked(move_idx_1882);

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
            (FEN_START, FEN_START_FLIPPED, 51),
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, -99),
            (FEN_MATE_IN_1_PROMOS_EP, FEN_MATE_IN_1_PROMOS_EP_FLIPPED, 84),
            (FEN_IN_CHECK, FEN_IN_CHECK_FLIPPED, -183),
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
            ("g1f3", (1.73, 0.37)),
            ("b1c3", (1.36, 0.26)),
            ("d2d3", (0.10, 0.07)),
            ("d2d4", (0.10, 0.07)),
            ("e2e4", (-0.14, 0.06)),
            ("c2c3", (-0.72, 0.03)),
            ("b1a3", (-0.76, 0.03)),
            ("c2c4", (-0.85, 0.03)),
            ("e2e3", (-1.02, 0.02)),
            ("a2a4", (-2.13, 0.01)),
            ("b2b4", (-2.29, 0.01)),
            ("f2f4", (-2.31, 0.01)),
            ("g1h3", (-2.32, 0.01)),
            ("a2a3", (-2.36, 0.01)),
            ("b2b3", (-2.47, 0.01)),
            ("g2g3", (-2.50, 0.01)),
            ("f2f3", (-2.99, 0.00)),
            ("h2h4", (-3.37, 0.00)),
            ("h2h3", (-4.64, 0.00)),
            ("g2g4", (-4.93, 0.00)),
        ]);

        let kiwipete_mod_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e5d3", (1.70, 0.23)),
            ("e5g6", (1.23, 0.14)),
            ("e2a6", (1.21, 0.14)),
            ("e5d7", (0.42, 0.06)),
            ("e1g1", (0.38, 0.06)),
            ("d5e6", (0.05, 0.04)),
            ("g2h3", (-0.22, 0.03)),
            ("f3h3", (-0.47, 0.03)),
            ("e5c6", (-0.59, 0.02)),
            ("b2b3", (-0.79, 0.02)),
            ("d2g5", (-0.98, 0.02)),
            ("e5g4", (-0.99, 0.02)),
            ("d2f4", (-1.05, 0.01)),
            ("c3d1", (-1.07, 0.01)),
            ("e5f7", (-1.14, 0.01)),
            ("g2g3", (-1.20, 0.01)),
            ("a1c1", (-1.26, 0.01)),
            ("c3a4", (-1.34, 0.01)),
            ("d2e3", (-1.37, 0.01)),
            ("f3g3", (-1.53, 0.01)),
            ("f3e3", (-1.61, 0.01)),
            ("a1d1", (-1.62, 0.01)),
            ("g2g4", (-1.66, 0.01)),
            ("a2a4", (-1.92, 0.01)),
            ("e2f1", (-1.93, 0.01)),
            ("a2a3", (-1.94, 0.01)),
            ("c3b5", (-1.98, 0.01)),
            ("d5d6", (-2.05, 0.01)),
            ("e2d3", (-2.13, 0.00)),
            ("f3f4", (-2.37, 0.00)),
            ("e2b5", (-2.38, 0.00)),
            ("f3d3", (-2.45, 0.00)),
            ("e5c4", (-2.62, 0.00)),
            ("f3f6", (-2.78, 0.00)),
            ("e2d1", (-3.01, 0.00)),
            ("e1d1", (-3.35, 0.00)),
            ("h1g1", (-3.48, 0.00)),
            ("d2h6", (-3.61, 0.00)),
            ("a1b1", (-3.79, 0.00)),
            ("d2c1", (-4.03, 0.00)),
            ("c3b1", (-4.20, 0.00)),
            ("f3g4", (-4.66, 0.00)),
            ("e1f1", (-5.33, 0.00)),
            ("e2c4", (-6.68, 0.00)),
            ("h1f1", (-6.79, 0.00)),
            ("f3f5", (-7.10, 0.00)),
            ("f3h5", (-7.91, 0.00)),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (3.11, 0.70)),
            ("f5g6", (1.11, 0.10)),
            ("f5f6", (0.03, 0.03)),
            ("e7d8n", (-0.29, 0.02)),
            ("c3a3", (-0.39, 0.02)),
            ("e7e8q", (-0.56, 0.02)),
            ("e7d8r", (-0.76, 0.01)),
            ("c3d3", (-0.81, 0.01)),
            ("c3e3", (-0.86, 0.01)),
            ("c3c5", (-1.20, 0.01)),
            ("e7d8b", (-1.21, 0.01)),
            ("c3c6", (-1.25, 0.01)),
            ("c3b3", (-1.32, 0.01)),
            ("c2b3", (-1.44, 0.01)),
            ("c3c4", (-1.50, 0.01)),
            ("c3f3", (-1.96, 0.00)),
            ("c2c1", (-2.12, 0.00)),
            ("c3c8", (-2.66, 0.00)),
            ("e7e8b", (-2.83, 0.00)),
            ("c3g3", (-2.91, 0.00)),
            ("e7e8r", (-3.13, 0.00)),
            ("c3h3", (-3.17, 0.00)),
            ("c3c7", (-3.21, 0.00)),
            ("e7e8n", (-3.56, 0.00)),
        ]);

        let in_check_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (2.88, 0.87)),
            ("b6c6", (-0.53, 0.03)),
            ("b6b7", (-0.58, 0.03)),
            ("e7d8n", (-0.79, 0.02)),
            ("e7d8r", (-0.98, 0.02)),
            ("e7d8b", (-1.40, 0.01)),
            ("b6b5", (-1.96, 0.01)),
            ("b6a7", (-2.04, 0.01)),
            ("b6c5", (-2.04, 0.01)),
            ("b6a6", (-2.12, 0.01)),
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
