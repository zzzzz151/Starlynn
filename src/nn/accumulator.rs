use super::params::{FT_Q, HL_SIZE, NET};
use crate::chess::{chess_move::ChessMove, pos_state::PosState, position::Position, types::*};
use debug_unwraps::DebugUnwrapExt;
use std::mem::transmute;
use strum::IntoEnumIterator;

fn get_feature_idx(
    acc_color: Color,
    king_sq: Square,
    in_check: bool,
    mut piece_color: Color,
    pt: PieceType,
    mut sq: Square,
) -> usize {
    // If black to move, flip pieces vertically
    if acc_color == Color::Black {
        piece_color = !piece_color;
        sq = sq.rank_flipped();
    }

    // If our king is on left side of board, mirror all pieces along vertical axis
    if king_sq.file() < File::E {
        sq = sq.file_flipped();
    }

    (in_check as usize) * 768 + (piece_color as usize) * 384 + (pt as usize) * 64 + (sq as usize)
}

#[repr(C, align(64))]
#[derive(Clone)]
pub struct BothAccumulators {
    unactivated_accs: [[i16; HL_SIZE]; 2],
    activated_accs: [[i16; HL_SIZE / 2]; 2],
    is_unactivated_updated: bool,
    is_activated_updated: bool,
}

impl BothAccumulators {
    pub const fn new() -> Self {
        BothAccumulators {
            unactivated_accs: [NET.hl_b, NET.hl_b],
            activated_accs: [[0; HL_SIZE / 2]; 2],
            is_unactivated_updated: true,
            is_activated_updated: false,
        }
    }

    pub fn build_accumulator(&mut self, acc_color: Color, pos: &Position) {
        self.unactivated_accs[acc_color] = NET.hl_b;

        let mut enable_feature =
            |acc_color: Color, pos: &Position, piece_color: Color, pt: PieceType, sq: Square| {
                let feature_idx: usize = get_feature_idx(
                    acc_color,
                    pos.king_square(acc_color),
                    pos.in_check(),
                    piece_color,
                    pt,
                    sq,
                );

                let ft_weights: &[i16; HL_SIZE] = unsafe { NET.ft_w.get_unchecked(feature_idx) };

                for (x, w) in self.unactivated_accs[acc_color].iter_mut().zip(ft_weights) {
                    *x += *w;
                }
            };

        for piece_color in [Color::White, Color::Black] {
            for pt in PieceType::iter() {
                for square in pos.piece_bb(piece_color, pt) {
                    enable_feature(acc_color, pos, piece_color, pt, square);
                }
            }
        }

        self.is_activated_updated = false;
    }

    pub fn build_accumulators(&mut self, pos: &Position) {
        for acc_color in [Color::White, Color::Black] {
            self.build_accumulator(acc_color, pos);
        }

        self.is_unactivated_updated = true;
    }

    pub fn set_not_updated(&mut self) {
        self.is_unactivated_updated = false;
        self.is_activated_updated = false;
    }

    // Activation is CReLU + pairwise mul
    pub fn activated_accs(&mut self) -> &[[i16; HL_SIZE / 2]; 2] {
        assert!(self.is_unactivated_updated);

        if self.is_activated_updated {
            return &self.activated_accs;
        }

        for (act_acc, unact_acc) in self.activated_accs.iter_mut().zip(&self.unactivated_accs) {
            for i in 0..(HL_SIZE / 2) {
                let x1: i16 = unact_acc[i * 2].clamp(0, FT_Q);
                let x2: i16 = unact_acc[i * 2 + 1].clamp(0, FT_Q);
                act_acc[i] = unsafe { x1.unchecked_mul(x2) };
            }
        }

        self.is_activated_updated = true;
        &self.activated_accs
    }

    pub fn update(&mut self, prev_accs: &BothAccumulators, pos_after_move: &Position) {
        debug_assert!(prev_accs.is_unactivated_updated);

        if self.is_unactivated_updated {
            debug_assert_eq!(
                &self.unactivated_accs,
                &BothAccumulators::from(pos_after_move).unactivated_accs
            );

            return;
        }

        let in_check: bool = pos_after_move.in_check();

        let state_moved: &PosState =
            unsafe { pos_after_move.state::<1>().debug_unwrap_unchecked() };

        // If in check input bucket changed, rebuild both accumulators
        if in_check != state_moved.in_check() {
            self.build_accumulator(Color::White, pos_after_move);
            self.build_accumulator(Color::Black, pos_after_move);
            self.is_unactivated_updated = true;
            return;
        }

        let stm: Color = state_moved.side_to_move();
        let mov: ChessMove = unsafe { pos_after_move.last_move().debug_unwrap_unchecked() };
        let src: Square = mov.src();
        let dst: Square = mov.dst();
        let pt_moved: PieceType = mov.piece_type();
        let pt_placed: PieceType = mov.promotion().unwrap_or(pt_moved);

        for acc_color in [Color::White, Color::Black] {
            let king_sq: Square = pos_after_move.king_square(acc_color);

            // If accumulator color's king crossed D to E or E to D, rebuild this accumulator
            if (king_sq.file() < File::E) != (state_moved.king_square(acc_color).file() < File::E) {
                self.build_accumulator(acc_color, pos_after_move);
                continue;
            }

            if mov.is_castling() {
                // Rook from to
                let (rook_src, rook_dst) = match dst {
                    Square::C1 => (Square::A1, Square::D1),
                    Square::G1 => (Square::H1, Square::F1),
                    Square::C8 => (Square::A8, Square::D8),
                    Square::G8 => (Square::H8, Square::F8),
                    _ => panic!("Invalid castling king target square"),
                };

                update_castling(
                    &mut self.unactivated_accs[acc_color],
                    &prev_accs.unactivated_accs[acc_color],
                    get_feature_idx(acc_color, king_sq, in_check, stm, PieceType::King, src),
                    get_feature_idx(acc_color, king_sq, in_check, stm, PieceType::King, dst),
                    get_feature_idx(acc_color, king_sq, in_check, stm, PieceType::Rook, rook_src),
                    get_feature_idx(acc_color, king_sq, in_check, stm, PieceType::Rook, rook_dst),
                );
            } else if let Some(pt_captured) = pos_after_move.piece_type_captured() {
                let captured_sq: Square = if pt_moved == PieceType::Pawn
                    && Some(dst) == state_moved.en_passant_square()
                {
                    unsafe { transmute(dst as u8 ^ 8) }
                } else {
                    dst
                };

                update_capture(
                    &mut self.unactivated_accs[acc_color],
                    &prev_accs.unactivated_accs[acc_color],
                    get_feature_idx(acc_color, king_sq, in_check, !stm, pt_captured, captured_sq),
                    get_feature_idx(acc_color, king_sq, in_check, stm, pt_placed, dst),
                    get_feature_idx(acc_color, king_sq, in_check, stm, pt_moved, src),
                );
            } else {
                update_non_capture(
                    &mut self.unactivated_accs[acc_color],
                    &prev_accs.unactivated_accs[acc_color],
                    get_feature_idx(acc_color, king_sq, in_check, stm, pt_moved, src),
                    get_feature_idx(acc_color, king_sq, in_check, stm, pt_placed, dst),
                );
            }
        }

        self.is_unactivated_updated = true;
        self.is_activated_updated = false;

        debug_assert_eq!(
            &self.unactivated_accs,
            &BothAccumulators::from(pos_after_move).unactivated_accs
        );
    }
}

impl From<&Position> for BothAccumulators {
    fn from(pos: &Position) -> Self {
        let mut both_accs = BothAccumulators::new();
        both_accs.build_accumulators(pos);
        both_accs
    }
}

fn update_castling(
    acc: &mut [i16; HL_SIZE],
    prev_acc: &[i16; HL_SIZE],
    sub_king_idx: usize,
    add_king_idx: usize,
    sub_rook_idx: usize,
    add_rook_idx: usize,
) {
    unsafe {
        for i in 0..HL_SIZE {
            acc[i] = prev_acc[i] - NET.ft_w.get_unchecked(sub_king_idx)[i]
                + NET.ft_w.get_unchecked(add_king_idx)[i]
                - NET.ft_w.get_unchecked(sub_rook_idx)[i]
                + NET.ft_w.get_unchecked(add_rook_idx)[i];
        }
    }
}

fn update_capture(
    acc: &mut [i16; HL_SIZE],
    prev_acc: &[i16; HL_SIZE],
    sub_captured_idx: usize,
    add_piece_idx: usize,
    sub_piece_idx: usize,
) {
    unsafe {
        for i in 0..HL_SIZE {
            acc[i] = prev_acc[i] - NET.ft_w.get_unchecked(sub_captured_idx)[i]
                + NET.ft_w.get_unchecked(add_piece_idx)[i]
                - NET.ft_w.get_unchecked(sub_piece_idx)[i];
        }
    }
}

fn update_non_capture(
    acc: &mut [i16; HL_SIZE],
    prev_acc: &[i16; HL_SIZE],
    sub_piece_idx: usize,
    add_piece_idx: usize,
) {
    unsafe {
        for i in 0..HL_SIZE {
            acc[i] = prev_acc[i] - NET.ft_w.get_unchecked(sub_piece_idx)[i]
                + NET.ft_w.get_unchecked(add_piece_idx)[i];
        }
    }
}
