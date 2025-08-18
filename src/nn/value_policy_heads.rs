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
            (FEN_START, FEN_START_FLIPPED, 78),
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, -59),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                181,
            ),
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
            ("g1f3", (1.01, 0.30)),
            ("b1c3", (0.94, 0.28)),
            ("e2e3", (-0.29, 0.08)),
            ("d2d4", (-0.35, 0.08)),
            ("e2e4", (-0.55, 0.06)),
            ("d2d3", (-0.91, 0.04)),
            ("c2c3", (-1.50, 0.02)),
            ("b2b3", (-1.65, 0.02)),
            ("c2c4", (-1.74, 0.02)),
            ("b1a3", (-2.00, 0.01)),
            ("f2f4", (-2.07, 0.01)),
            ("b2b4", (-2.08, 0.01)),
            ("g2g3", (-2.15, 0.01)),
            ("g1h3", (-2.35, 0.01)),
            ("a2a4", (-2.63, 0.01)),
            ("h2h4", (-3.00, 0.01)),
            ("f2f3", (-3.16, 0.00)),
            ("a2a3", (-3.18, 0.00)),
            ("g2g4", (-3.39, 0.00)),
            ("h2h3", (-4.36, 0.00)),
        ]);

        let kiwipete_mod_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("c3d1", (1.26, 0.18)),
            ("e2a6", (1.13, 0.16)),
            ("f3h3", (0.37, 0.07)),
            ("e5c6", (0.37, 0.07)),
            ("g2h3", (0.30, 0.07)),
            ("e5d7", (0.23, 0.06)),
            ("d5e6", (0.11, 0.06)),
            ("f3g3", (-0.29, 0.04)),
            ("e5d3", (-0.50, 0.03)),
            ("c3b5", (-0.79, 0.02)),
            ("c3a4", (-0.97, 0.02)),
            ("d2f4", (-1.01, 0.02)),
            ("e5g6", (-1.02, 0.02)),
            ("e5g4", (-1.12, 0.02)),
            ("c3b1", (-1.23, 0.02)),
            ("g2g3", (-1.26, 0.01)),
            ("f3e3", (-1.30, 0.01)),
            ("d2g5", (-1.42, 0.01)),
            ("d2e3", (-1.45, 0.01)),
            ("e1g1", (-1.51, 0.01)),
            ("f3f4", (-1.64, 0.01)),
            ("d5d6", (-2.08, 0.01)),
            ("e2b5", (-2.20, 0.01)),
            ("a2a4", (-2.21, 0.01)),
            ("e2d3", (-2.31, 0.01)),
            ("e2f1", (-2.31, 0.01)),
            ("a1d1", (-2.35, 0.00)),
            ("e5c4", (-2.37, 0.00)),
            ("g2g4", (-2.37, 0.00)),
            ("b2b3", (-2.43, 0.00)),
            ("e5f7", (-2.56, 0.00)),
            ("f3d3", (-2.81, 0.00)),
            ("f3f6", (-2.84, 0.00)),
            ("e2d1", (-3.13, 0.00)),
            ("h1g1", (-3.16, 0.00)),
            ("a1c1", (-3.54, 0.00)),
            ("a2a3", (-3.78, 0.00)),
            ("a1b1", (-4.00, 0.00)),
            ("d2h6", (-4.52, 0.00)),
            ("d2c1", (-4.71, 0.00)),
            ("e1d1", (-4.81, 0.00)),
            ("f3g4", (-5.02, 0.00)),
            ("e1f1", (-5.23, 0.00)),
            ("e2c4", (-5.78, 0.00)),
            ("h1f1", (-6.58, 0.00)),
            ("f3f5", (-6.87, 0.00)),
            ("f3h5", (-7.60, 0.00)),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("f7g8q", (3.13, 0.58)),
            ("c5d6", (1.53, 0.12)),
            ("f7f8q", (0.88, 0.06)),
            ("g3g8", (0.55, 0.04)),
            ("c5c6", (0.21, 0.03)),
            ("f7g8n", (-0.23, 0.02)),
            ("f1e2", (-0.35, 0.02)),
            ("f1f2", (-0.40, 0.02)),
            ("g3e3", (-0.44, 0.02)),
            ("g3h3", (-0.70, 0.01)),
            ("g3f3", (-0.76, 0.01)),
            ("f7g8r", (-0.78, 0.01)),
            ("f1e1", (-1.20, 0.01)),
            ("f7g8b", (-1.25, 0.01)),
            ("g3d3", (-1.34, 0.01)),
            ("g3g5", (-1.39, 0.01)),
            ("g3g4", (-1.58, 0.01)),
            ("g3c3", (-1.70, 0.00)),
            ("g3g6", (-1.74, 0.00)),
            ("g3a3", (-1.95, 0.00)),
            ("g3g2", (-2.21, 0.00)),
            ("g3b3", (-2.25, 0.00)),
            ("g3g1", (-2.43, 0.00)),
            ("g3g7", (-2.48, 0.00)),
            ("f7f8n", (-2.75, 0.00)),
            ("f7f8r", (-2.97, 0.00)),
            ("f7f8b", (-3.91, 0.00)),
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
        ] {
            assert_policy(fen, expected_policy);
            assert_policy(fen_flipped, expected_policy);
        }
    }
}
