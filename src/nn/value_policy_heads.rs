use super::accumulator::BothAccumulators;
use super::moves_map::get_move_idx_1882;
use super::params::*;
use arrayvec::ArrayVec;

use crate::chess::{
    chess_move::ChessMove,
    move_gen::MovesList,
    position::Position,
    types::{Color, Square},
};

pub type ScoredMoves = ArrayVec<(ChessMove, f32), 256>;

pub fn value_eval(both_accs: &mut BothAccumulators, stm: Color) -> i32 {
    let hl_activated: &[[i16; HALF_HL_SIZE]; 2] = both_accs.activated_accs();

    let mut result: f32 = 0.0;

    for (is_nstm, &color) in [stm, !stm].iter().enumerate() {
        for i in 0..HALF_HL_SIZE {
            result += hl_activated[color][i] as f32 * NET.out_w_value[is_nstm][i];
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

    for &mov in legal_moves {
        if Some(mov) == exclude_move {
            continue;
        }

        // In quiescence search, skip quiets, underpromotions and moves that lose material
        if Q_SEARCH && (pos.is_quiet_or_underpromotion(mov) || !pos.see_ge(mov, 0)) {
            continue;
        }

        let hl_activated: &[[i16; HALF_HL_SIZE]; 2] = both_accs.activated_accs();
        let mut move_oriented: ChessMove = mov;

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

        let move_idx_1882: usize = get_move_idx_1882(move_oriented);
        let mut logit: f32 = 0.0;

        unsafe {
            let out_w: &[[f32; HALF_HL_SIZE]; 2] = NET.out_w_policy.get_unchecked(move_idx_1882);

            for (is_nstm, &color) in [pos.side_to_move(), !pos.side_to_move()].iter().enumerate() {
                for i in 0..HALF_HL_SIZE {
                    logit += hl_activated[color][i] as f32 * out_w[is_nstm][i];
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
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w Kq - 0 1";

    const FEN_KIWIPETE_MODIFIED_FLIPPED: &str =
        "r3k2r/pppbbppp/2n2q1P/1P2p3/3pn3/BN2PNP1/P1PPQPB1/R3K2R b Qk - 0 1";

    const FEN_MATE_IN_1_PROMOS_EP: &str = "6q1/5P2/8/2Pp4/8/6R1/8/5K1k w - d6 0 1";
    const FEN_MATE_IN_1_PROMOS_EP_FLIPPED: &str = "5k1K/8/6r1/8/2pP4/8/5p2/6Q1 b - d3 0 1";

    const FEN_IN_CHECK: &str = "4r1k1/3P4/3N4/8/8/8/8/4K2R w K - 0 1";
    const FEN_IN_CHECK_FLIPPED: &str = "4k2r/8/8/8/8/3n4/3p4/4R1K1 b k - 0 1";

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
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, -111),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                -11,
            ),
            (FEN_IN_CHECK, FEN_IN_CHECK_FLIPPED, 755),
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
            ("g1f3", (2.01, 0.39)),
            ("b1c3", (1.58, 0.25)),
            ("e2e4", (0.32, 0.07)),
            ("d2d4", (0.10, 0.06)),
            ("e2e3", (-0.24, 0.04)),
            ("d2d3", (-0.44, 0.03)),
            ("c2c3", (-0.54, 0.03)),
            ("b1a3", (-0.91, 0.02)),
            ("g2g3", (-1.25, 0.01)),
            ("b2b3", (-1.29, 0.01)),
            ("g1h3", (-1.29, 0.01)),
            ("c2c4", (-1.44, 0.01)),
            ("b2b4", (-1.49, 0.01)),
            ("a2a4", (-1.58, 0.01)),
            ("f2f4", (-1.59, 0.01)),
            ("f2f3", (-2.03, 0.01)),
            ("h2h4", (-2.29, 0.01)),
            ("a2a3", (-2.47, 0.00)),
            ("g2g4", (-2.71, 0.00)),
            ("h2h3", (-4.05, 0.00)),
        ]);

        let kiwipete_mod_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("c3d1", (2.66, 0.44)),
            ("e2a6", (1.17, 0.10)),
            ("d5e6", (0.54, 0.05)),
            ("c3a4", (0.46, 0.05)),
            ("c3b5", (0.30, 0.04)),
            ("f3h3", (0.20, 0.04)),
            ("e5d3", (0.08, 0.03)),
            ("e5c6", (-0.01, 0.03)),
            ("c3b1", (-0.21, 0.02)),
            ("g2h3", (-0.35, 0.02)),
            ("e5g6", (-0.66, 0.02)),
            ("f3g3", (-0.67, 0.02)),
            ("g2g3", (-0.73, 0.01)),
            ("e1g1", (-0.90, 0.01)),
            ("d2f4", (-1.01, 0.01)),
            ("b2b3", (-1.02, 0.01)),
            ("e5g4", (-1.09, 0.01)),
            ("e5d7", (-1.27, 0.01)),
            ("g2g4", (-1.36, 0.01)),
            ("f3e3", (-1.39, 0.01)),
            ("f3f4", (-1.43, 0.01)),
            ("f3d3", (-1.50, 0.01)),
            ("d2g5", (-1.72, 0.01)),
            ("e2d3", (-1.77, 0.01)),
            ("d5d6", (-1.87, 0.00)),
            ("d2e3", (-1.89, 0.00)),
            ("a2a4", (-1.92, 0.00)),
            ("a1d1", (-2.20, 0.00)),
            ("e2d1", (-2.30, 0.00)),
            ("e2b5", (-2.37, 0.00)),
            ("e5c4", (-2.38, 0.00)),
            ("a2a3", (-2.56, 0.00)),
            ("d2c1", (-2.60, 0.00)),
            ("e5f7", (-2.78, 0.00)),
            ("e2f1", (-3.08, 0.00)),
            ("a1c1", (-3.29, 0.00)),
            ("a1b1", (-3.36, 0.00)),
            ("f3f6", (-3.43, 0.00)),
            ("f3g4", (-3.82, 0.00)),
            ("h1g1", (-3.82, 0.00)),
            ("d2h6", (-4.44, 0.00)),
            ("e1d1", (-5.24, 0.00)),
            ("e1f1", (-5.43, 0.00)),
            ("e2c4", (-5.57, 0.00)),
            ("f3f5", (-6.64, 0.00)),
            ("h1f1", (-6.83, 0.00)),
            ("f3h5", (-7.38, 0.00)),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("f7g8q", (2.77, 0.53)),
            ("g3g8", (1.67, 0.17)),
            ("c5c6", (0.70, 0.07)),
            ("c5d6", (0.57, 0.06)),
            ("f1f2", (-0.19, 0.03)),
            ("f7g8n", (-0.68, 0.02)),
            ("f1e2", (-0.78, 0.02)),
            ("g3h3", (-0.79, 0.01)),
            ("g3e3", (-0.84, 0.01)),
            ("f7f8q", (-0.85, 0.01)),
            ("g3f3", (-1.03, 0.01)),
            ("f1e1", (-1.06, 0.01)),
            ("f7g8r", (-1.22, 0.01)),
            ("f7g8b", (-1.52, 0.01)),
            ("g3g4", (-1.82, 0.01)),
            ("g3g5", (-2.05, 0.00)),
            ("g3c3", (-2.35, 0.00)),
            ("g3g2", (-2.38, 0.00)),
            ("g3a3", (-2.57, 0.00)),
            ("g3d3", (-2.61, 0.00)),
            ("g3b3", (-2.62, 0.00)),
            ("g3g7", (-2.70, 0.00)),
            ("g3g6", (-2.77, 0.00)),
            ("f7f8r", (-3.26, 0.00)),
            ("g3g1", (-3.42, 0.00)),
            ("f7f8b", (-3.56, 0.00)),
            ("f7f8n", (-4.28, 0.00)),
        ]);

        let in_check_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("d7e8q", (2.95, 0.63)),
            ("d6e8", (2.10, 0.27)),
            ("d7e8n", (-0.11, 0.03)),
            ("e1f2", (-0.67, 0.02)),
            ("d7e8r", (-0.90, 0.01)),
            ("d7e8b", (-0.91, 0.01)),
            ("e1d1", (-1.15, 0.01)),
            ("e1d2", (-1.61, 0.01)),
            ("d6e4", (-2.73, 0.00)),
            ("e1f1", (-2.78, 0.00)),
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
