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
            (FEN_START, FEN_START_FLIPPED, 82),
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, 258),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                338,
            ),
            (FEN_IN_CHECK, FEN_IN_CHECK_FLIPPED, 15),
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
            ("d2d4", (11.39, 0.81)),
            ("g1f3", (9.88, 0.18)),
            ("b1c3", (6.30, 0.01)),
            ("e2e3", (5.95, 0.00)),
            ("e2e4", (4.33, 0.00)),
            ("g1h3", (-2.06, 0.00)),
            ("b1a3", (-4.91, 0.00)),
            ("d2d3", (-7.25, 0.00)),
            ("g2g3", (-8.51, 0.00)),
            ("b2b3", (-9.98, 0.00)),
            ("f2f3", (-10.44, 0.00)),
            ("c2c3", (-11.09, 0.00)),
            ("a2a3", (-11.18, 0.00)),
            ("h2h4", (-11.95, 0.00)),
            ("g2g4", (-11.97, 0.00)),
            ("c2c4", (-12.18, 0.00)),
            ("a2a4", (-13.17, 0.00)),
            ("b2b4", (-13.90, 0.00)),
            ("h2h3", (-14.22, 0.00)),
            ("f2f4", (-15.29, 0.00)),
        ]);

        let kiwipete_mod_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e1g1", (5.63, 0.89)),
            ("d5e6", (2.91, 0.06)),
            ("a1d1", (2.11, 0.03)),
            ("f3g4", (2.05, 0.02)),
            ("e5d3", (-0.34, 0.00)),
            ("a1c1", (-1.22, 0.00)),
            ("d5d6", (-2.59, 0.00)),
            ("e2c4", (-3.13, 0.00)),
            ("f3f6", (-3.80, 0.00)),
            ("g2h3", (-3.90, 0.00)),
            ("d2f4", (-4.46, 0.00)),
            ("e2d3", (-4.97, 0.00)),
            ("f3f5", (-5.01, 0.00)),
            ("a1b1", (-5.20, 0.00)),
            ("h1f1", (-5.21, 0.00)),
            ("d2e3", (-6.28, 0.00)),
            ("h1g1", (-6.57, 0.00)),
            ("e5g4", (-6.59, 0.00)),
            ("g2g3", (-6.91, 0.00)),
            ("e5c4", (-7.07, 0.00)),
            ("f3h3", (-7.13, 0.00)),
            ("c3a4", (-7.20, 0.00)),
            ("d2c1", (-7.32, 0.00)),
            ("f3d3", (-7.40, 0.00)),
            ("f3g3", (-7.69, 0.00)),
            ("a2a3", (-7.73, 0.00)),
            ("e1f1", (-7.96, 0.00)),
            ("b2b3", (-7.99, 0.00)),
            ("e5g6", (-8.32, 0.00)),
            ("e5c6", (-8.50, 0.00)),
            ("c3b1", (-8.58, 0.00)),
            ("c3d1", (-8.77, 0.00)),
            ("e2f1", (-8.87, 0.00)),
            ("c3b5", (-8.97, 0.00)),
            ("e2a6", (-8.98, 0.00)),
            ("f3e3", (-9.03, 0.00)),
            ("e5d7", (-9.23, 0.00)),
            ("d2h6", (-9.26, 0.00)),
            ("e1d1", (-9.43, 0.00)),
            ("e2b5", (-9.57, 0.00)),
            ("e5f7", (-9.76, 0.00)),
            ("f3h5", (-9.93, 0.00)),
            ("e2d1", (-10.96, 0.00)),
            ("g2g4", (-11.24, 0.00)),
            ("d2g5", (-11.43, 0.00)),
            ("a2a4", (-12.11, 0.00)),
            ("f3f4", (-13.33, 0.00)),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (13.37, 0.94)),
            ("e7e8q", (10.59, 0.06)),
            ("f5f6", (7.11, 0.00)),
            ("c2b3", (2.32, 0.00)),
            ("c3c5", (0.90, 0.00)),
            ("e7d8r", (-0.02, 0.00)),
            ("e7d8b", (-0.02, 0.00)),
            ("e7d8n", (-0.02, 0.00)),
            ("c3b3", (-1.46, 0.00)),
            ("c3c7", (-1.96, 0.00)),
            ("f5g6", (-1.99, 0.00)),
            ("c3d3", (-2.64, 0.00)),
            ("c2c1", (-2.98, 0.00)),
            ("c3c6", (-3.22, 0.00)),
            ("e7e8r", (-4.04, 0.00)),
            ("e7e8n", (-4.20, 0.00)),
            ("e7e8b", (-4.21, 0.00)),
            ("c3c8", (-4.45, 0.00)),
            ("c3a3", (-6.07, 0.00)),
            ("c3e3", (-6.35, 0.00)),
            ("c3h3", (-6.73, 0.00)),
            ("c3g3", (-6.82, 0.00)),
            ("c3f3", (-7.35, 0.00)),
            ("c3c4", (-8.60, 0.00)),
        ]);

        let in_check_policy: HashMap<&str, (f32, f32)> = HashMap::from([
            ("e7d8q", (8.50, 0.57)),
            ("b6b7", (8.19, 0.42)),
            ("b6c6", (3.08, 0.00)),
            ("b6c5", (0.74, 0.00)),
            ("b6b5", (0.60, 0.00)),
            ("b6a7", (0.11, 0.00)),
            ("e7d8r", (-0.01, 0.00)),
            ("e7d8b", (-0.02, 0.00)),
            ("e7d8n", (-0.02, 0.00)),
            ("b6a6", (-1.69, 0.00)),
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
