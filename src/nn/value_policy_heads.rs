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
            let out_w: &[[f32; HL_SIZE / 2]; 2] = NET.out_w_policy.get_unchecked(move_idx_1882);

            for (is_nstm, &color) in [pos.side_to_move(), !pos.side_to_move()].iter().enumerate() {
                for i in 0..(HL_SIZE / 2) {
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
            (FEN_START, FEN_START_FLIPPED, 42),
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, 123),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                -62,
            ),
            (FEN_IN_CHECK, FEN_IN_CHECK_FLIPPED, -191),
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
            ("b1c3", (2.06, 0.27)),
            ("g1f3", (1.83, 0.21)),
            ("d2d4", (1.36, 0.13)),
            ("d2d3", (1.07, 0.10)),
            ("e2e4", (0.99, 0.09)),
            ("c2c3", (0.42, 0.05)),
            ("e2e3", (0.22, 0.04)),
            ("c2c4", (-0.41, 0.02)),
            ("b2b3", (-0.89, 0.01)),
            ("b1a3", (-1.06, 0.01)),
            ("b2b4", (-1.40, 0.01)),
            ("a2a3", (-1.57, 0.01)),
            ("a2a4", (-1.74, 0.01)),
            ("g1h3", (-1.78, 0.01)),
            ("f2f3", (-1.86, 0.01)),
            ("f2f4", (-1.89, 0.01)),
            ("g2g3", (-1.94, 0.00)),
            ("g2g4", (-2.69, 0.00)),
            ("h2h3", (-2.73, 0.00)),
            ("h2h4", (-3.05, 0.00)),
        ]);

        let kiwipete_mod_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e5c6", (1.18, 0.12)),
            ("g2h3", (1.18, 0.12)),
            ("c3d1", (0.85, 0.09)),
            ("f3g3", (0.66, 0.07)),
            ("f3h3", (0.61, 0.07)),
            ("e5d3", (0.43, 0.06)),
            ("g2g3", (0.40, 0.05)),
            ("d2f4", (0.15, 0.04)),
            ("e2a6", (0.13, 0.04)),
            ("e5g4", (0.04, 0.04)),
            ("d5e6", (-0.04, 0.04)),
            ("a2a4", (-0.22, 0.03)),
            ("d5d6", (-0.27, 0.03)),
            ("e1g1", (-0.41, 0.02)),
            ("e5g6", (-0.58, 0.02)),
            ("d2g5", (-0.59, 0.02)),
            ("a1d1", (-0.80, 0.02)),
            ("b2b3", (-0.88, 0.02)),
            ("e5f7", (-0.88, 0.02)),
            ("e2d3", (-1.44, 0.01)),
            ("e2d1", (-1.45, 0.01)),
            ("f3f4", (-1.47, 0.01)),
            ("e5c4", (-1.58, 0.01)),
            ("c3b5", (-1.63, 0.01)),
            ("d2e3", (-1.71, 0.01)),
            ("f3e3", (-1.79, 0.01)),
            ("g2g4", (-1.92, 0.01)),
            ("c3b1", (-2.09, 0.00)),
            ("c3a4", (-2.21, 0.00)),
            ("f3d3", (-2.30, 0.00)),
            ("a2a3", (-2.42, 0.00)),
            ("e2b5", (-2.51, 0.00)),
            ("e2f1", (-2.57, 0.00)),
            ("e5d7", (-2.64, 0.00)),
            ("a1c1", (-2.69, 0.00)),
            ("f3g4", (-2.80, 0.00)),
            ("h1g1", (-2.93, 0.00)),
            ("d2h6", (-3.02, 0.00)),
            ("f3f6", (-3.13, 0.00)),
            ("d2c1", (-3.27, 0.00)),
            ("a1b1", (-3.29, 0.00)),
            ("e1d1", (-3.41, 0.00)),
            ("e2c4", (-3.69, 0.00)),
            ("e1f1", (-3.70, 0.00)),
            ("h1f1", (-5.08, 0.00)),
            ("f3f5", (-5.70, 0.00)),
            ("f3h5", (-6.30, 0.00)),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (2.79, 0.52)),
            ("e7e8q", (1.31, 0.12)),
            ("f5g6", (1.23, 0.11)),
            ("e7d8n", (0.28, 0.04)),
            ("f5f6", (0.28, 0.04)),
            ("e7d8r", (-0.26, 0.02)),
            ("c3c4", (-0.60, 0.02)),
            ("c3d3", (-0.65, 0.02)),
            ("c2b3", (-0.77, 0.01)),
            ("c3e3", (-0.93, 0.01)),
            ("e7d8b", (-0.99, 0.01)),
            ("c3a3", (-1.14, 0.01)),
            ("c3c8", (-1.16, 0.01)),
            ("c3c6", (-1.18, 0.01)),
            ("c3c5", (-1.51, 0.01)),
            ("c3b3", (-1.52, 0.01)),
            ("c2c1", (-1.98, 0.00)),
            ("e7e8r", (-2.00, 0.00)),
            ("c3f3", (-2.08, 0.00)),
            ("c3g3", (-2.22, 0.00)),
            ("c3h3", (-2.55, 0.00)),
            ("e7e8n", (-3.06, 0.00)),
            ("c3c7", (-3.26, 0.00)),
            ("e7e8b", (-3.31, 0.00)),
        ]);

        let in_check_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (1.91, 0.38)),
            ("b6b7", (1.41, 0.23)),
            ("b6c6", (0.87, 0.13)),
            ("b6c5", (-0.19, 0.05)),
            ("b6b5", (-0.24, 0.04)),
            ("b6a7", (-0.24, 0.04)),
            ("e7d8n", (-0.33, 0.04)),
            ("e7d8r", (-0.33, 0.04)),
            ("e7d8b", (-0.73, 0.03)),
            ("b6a6", (-1.63, 0.01)),
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
