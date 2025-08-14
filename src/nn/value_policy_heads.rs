use super::params::*;
use super::{accumulator::BothAccumulators, moves_map::get_move_idx_1882};
use crate::chess::{chess_move::ChessMove, types::Color, types::Square};
use arrayvec::ArrayVec;

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

pub fn get_policy_logits(
    both_accs: &mut BothAccumulators,
    stm: Color,
    legal_moves: &ArrayVec<ChessMove, 256>,
) -> ArrayVec<(ChessMove, f32), 256> {
    let hl_activated: &[[i16; HALF_HL_SIZE]; 2] = both_accs.activated_accs();

    let mut policy: ArrayVec<(ChessMove, f32), 256> = ArrayVec::new();

    for &mov in legal_moves {
        let mut move_oriented: ChessMove = mov;

        // Flip move vertically if black to move
        if stm == Color::Black {
            let new_src: Square = mov.src().rank_flipped();
            let new_dst: Square = mov.dst().rank_flipped();

            if let Some(promo_pt) = mov.promotion() {
                move_oriented = ChessMove::new_promotion(new_src, new_dst, promo_pt);
            } else {
                move_oriented = ChessMove::new(new_src, new_dst, mov.piece_type());
            }
        }

        let move_idx_1882: usize = get_move_idx_1882(move_oriented);
        let mut move_policy: f32 = 0.0;

        let out_w: &[[f32; HALF_HL_SIZE]; 2] =
            unsafe { NET.out_w_policy.get_unchecked(move_idx_1882) };

        for (is_nstm, &color) in [stm, !stm].iter().enumerate() {
            for i in 0..HALF_HL_SIZE {
                move_policy += hl_activated[color][i] as f32 * out_w[is_nstm][i];
            }
        }

        move_policy /= (FT_Q * FT_Q) as f32;
        move_policy += unsafe { *NET.out_b_policy.get_unchecked(move_idx_1882) };

        unsafe {
            policy.push_unchecked((mov, move_policy));
        }
    }

    policy
}

pub fn softmax(policy_logits: &mut ArrayVec<(ChessMove, f32), 256>) {
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
            (FEN_START, FEN_START_FLIPPED, 83),
            (FEN_KIWIPETE_MODIFIED, FEN_KIWIPETE_MODIFIED_FLIPPED, 77),
            (
                FEN_MATE_IN_1_PROMOS_EP,
                FEN_MATE_IN_1_PROMOS_EP_FLIPPED,
                -138,
            ),
        ] {
            assert_value_eval(fen, expected_eval);
            assert_value_eval(fen_flipped, expected_eval);
        }
    }

    #[test]
    fn test_policy() {
        let start_pos_policy: HashMap<&str, f32> = HashMap::from([
            ("d2d4", 0.24),
            ("g1f3", 0.24),
            ("b1c3", 0.15),
            ("d2d3", 0.10),
            ("e2e4", 0.09),
            ("e2e3", 0.07),
            ("c2c3", 0.04),
            ("c2c4", 0.02),
            ("b1a3", 0.01),
            ("g1h3", 0.01),
            ("f2f4", 0.01),
            ("a2a3", 0.00),
            ("a2a4", 0.00),
            ("b2b3", 0.00),
            ("g2g3", 0.00),
            ("f2f3", 0.00),
            ("g2g4", 0.00),
            ("b2b4", 0.00),
            ("h2h4", 0.00),
            ("h2h3", 0.00),
        ]);

        let kiwipete_mod_policy: HashMap<&str, f32> = HashMap::from([
            ("e2a6", 0.23),
            ("g2g3", 0.16),
            ("c3d1", 0.08),
            ("g2h3", 0.08),
            ("f3h3", 0.05),
            ("e5d3", 0.04),
            ("f3g3", 0.04),
            ("g2g4", 0.04),
            ("c3b5", 0.03),
            ("d5e6", 0.03),
            ("e5g6", 0.02),
            ("e5g4", 0.02),
            ("e2b5", 0.02),
            ("c3a4", 0.01),
            ("f3e3", 0.01),
            ("f3f4", 0.01),
            ("d2g5", 0.01),
            ("e1g1", 0.01),
            ("a2a3", 0.01),
            ("c3b1", 0.01),
            ("d5d6", 0.01),
            ("d2e3", 0.01),
            ("e5d7", 0.01),
            ("d2f4", 0.01),
            ("b2b3", 0.01),
            ("e5c6", 0.01),
            ("a1c1", 0.01),
            ("e2d1", 0.00),
            ("e2d3", 0.00),
            ("a1d1", 0.00),
            ("f3d3", 0.00),
            ("e5c4", 0.00),
            ("e5f7", 0.00),
            ("e2f1", 0.00),
            ("h1g1", 0.00),
            ("a2a4", 0.00),
            ("d2c1", 0.00),
            ("a1b1", 0.00),
            ("f3g4", 0.00),
            ("e1f1", 0.00),
            ("e1d1", 0.00),
            ("f3f6", 0.00),
            ("d2h6", 0.00),
            ("h1f1", 0.00),
            ("e2c4", 0.00),
            ("f3h5", 0.00),
            ("f3f5", 0.00),
        ]);

        let mate_in_1_promos_ep_policy: HashMap<&str, f32> = HashMap::from([
            ("f7g8q", 0.46),
            ("g3g8", 0.31),
            ("c5d6", 0.06),
            ("c5c6", 0.03),
            ("f7g8r", 0.02),
            ("f7g8n", 0.02),
            ("f1f2", 0.02),
            ("g3h3", 0.01),
            ("f7g8b", 0.01),
            ("f7f8q", 0.01),
            ("g3f3", 0.01),
            ("g3e3", 0.01),
            ("f1e2", 0.00),
            ("g3d3", 0.00),
            ("g3g5", 0.00),
            ("g3a3", 0.00),
            ("f1e1", 0.00),
            ("g3g4", 0.00),
            ("g3g6", 0.00),
            ("g3c3", 0.00),
            ("f7f8n", 0.00),
            ("g3g2", 0.00),
            ("f7f8b", 0.00),
            ("g3g1", 0.00),
            ("g3b3", 0.00),
            ("f7f8r", 0.00),
            ("g3g7", 0.00),
        ]);

        let assert_policy = |fen: &str, expected_policy: &HashMap<&str, f32>| {
            let pos = Position::try_from(fen).unwrap();
            let mut both_accs = BothAccumulators::from(&pos);

            let mut policy =
                get_policy_logits(&mut both_accs, pos.side_to_move(), &pos.legal_moves());

            assert_eq!(policy.len(), expected_policy.len());

            softmax(&mut policy);

            for (mov, move_policy) in policy {
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

                let expected_move_policy: f32 = *expected_policy
                    .get(move_oriented.to_string().as_str())
                    .unwrap_or_else(|| panic!("Move {mov} not in expected policy"));

                assert!(
                    (move_policy - expected_move_policy).abs() < 0.005,
                    "{fen}\n{mov} {move_policy} {expected_move_policy}"
                );
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
