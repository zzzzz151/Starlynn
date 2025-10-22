use super::params::{FT_Q, HL_SIZE, NET};
use crate::chess::{chess_move::ChessMove, pos_state::PosState, position::Position, types::*};
use debug_unwraps::DebugUnwrapExt;
use std::mem::{MaybeUninit, transmute};
use strum::IntoEnumIterator;

#[repr(C, align(64))]
#[derive(Clone)]
pub struct Accumulator<const IS_WHITE_POV: bool>([i16; HL_SIZE]);

impl<const IS_WHITE_POV: bool> Accumulator<IS_WHITE_POV> {
    pub const fn new() -> Self {
        Accumulator(NET.hl_b)
    }

    const fn pov(&self) -> Color {
        [Color::Black, Color::White][IS_WHITE_POV as usize]
    }

    // Activation is CReLU + pairwise mul
    pub fn activated(&self) -> [i16; HL_SIZE / 2] {
        let mut result: [MaybeUninit<i16>; HL_SIZE / 2] =
            [const { MaybeUninit::uninit() }; HL_SIZE / 2];

        for (i, result_elem) in result.iter_mut().enumerate() {
            let x1: i16 = self.0[i * 2].clamp(0, FT_Q);
            let x2: i16 = self.0[i * 2 + 1].clamp(0, FT_Q);

            unsafe {
                result_elem.write(x1.unchecked_mul(x2));
            }
        }

        unsafe { transmute(result) }
    }

    pub fn update(&mut self, prev_acc: &Accumulator<IS_WHITE_POV>, pos_after_move: &Position) {
        let pos_before_move: &PosState =
            unsafe { pos_after_move.state::<1>().debug_unwrap_unchecked() };

        let in_check: bool = pos_after_move.in_check();

        let prev_king_sq: Square = pos_before_move.king_square(self.pov());
        let king_sq: Square = pos_after_move.king_square(self.pov());

        // If in-check input bucket changed or pov's king crossed vertical axis,
        // rebuild accumulator
        if in_check != pos_before_move.in_check()
            || (king_sq.file() < File::E) != (prev_king_sq.file() < File::E)
        {
            *self = Accumulator::from(pos_after_move);
            return;
        }

        let stm: Color = pos_before_move.side_to_move();
        let mov: ChessMove = unsafe { pos_after_move.last_move().debug_unwrap_unchecked() };
        let src: Square = mov.src();
        let dst: Square = mov.dst();
        let pt_moved: PieceType = mov.piece_type();
        let pt_placed: PieceType = mov.promotion().unwrap_or(pt_moved);

        if mov.is_castling() {
            // Rook from to
            let (rook_src, rook_dst) = match dst {
                Square::C1 => (Square::A1, Square::D1),
                Square::G1 => (Square::H1, Square::F1),
                Square::C8 => (Square::A8, Square::D8),
                Square::G8 => (Square::H8, Square::F8),
                _ => panic!("Invalid castling king target square"),
            };

            self.update_castling(
                prev_acc,
                self.get_feature_idx(in_check, king_sq, stm, PieceType::King, src),
                self.get_feature_idx(in_check, king_sq, stm, PieceType::King, dst),
                self.get_feature_idx(in_check, king_sq, stm, PieceType::Rook, rook_src),
                self.get_feature_idx(in_check, king_sq, stm, PieceType::Rook, rook_dst),
            );
        } else if let Some(pt_captured) = pos_after_move.piece_type_captured() {
            let captured_sq: Square = if pt_moved == PieceType::Pawn
                && Some(dst) == pos_before_move.en_passant_square()
            {
                unsafe { transmute(dst as u8 ^ 8) }
            } else {
                dst
            };

            self.update_capture(
                prev_acc,
                self.get_feature_idx(in_check, king_sq, !stm, pt_captured, captured_sq),
                self.get_feature_idx(in_check, king_sq, stm, pt_placed, dst),
                self.get_feature_idx(in_check, king_sq, stm, pt_moved, src),
            );
        } else {
            self.update_non_capture(
                prev_acc,
                self.get_feature_idx(in_check, king_sq, stm, pt_moved, src),
                self.get_feature_idx(in_check, king_sq, stm, pt_placed, dst),
            );
        }

        debug_assert!(self.0 == Accumulator::<IS_WHITE_POV>::from(pos_after_move).0);
    }

    fn get_feature_idx(
        &self,
        in_check: bool,
        king_sq: Square,
        mut piece_color: Color,
        pt: PieceType,
        mut sq: Square,
    ) -> usize {
        // If black to move, flip pieces vertically
        if !IS_WHITE_POV {
            piece_color = !piece_color;
            sq = sq.rank_flipped();
        }

        // If our king is on left side of board, mirror all pieces along vertical axis
        if king_sq.file() < File::E {
            sq = sq.file_flipped();
        }

        (in_check as usize) * 768
            + (piece_color as usize) * 384
            + (pt as usize) * 64
            + (sq as usize)
    }

    fn update_castling(
        &mut self,
        prev_acc: &Accumulator<IS_WHITE_POV>,
        sub_king_idx: usize,
        add_king_idx: usize,
        sub_rook_idx: usize,
        add_rook_idx: usize,
    ) {
        unsafe {
            for i in 0..HL_SIZE {
                self.0[i] = prev_acc.0[i] - NET.ft_w.get_unchecked(sub_king_idx)[i]
                    + NET.ft_w.get_unchecked(add_king_idx)[i]
                    - NET.ft_w.get_unchecked(sub_rook_idx)[i]
                    + NET.ft_w.get_unchecked(add_rook_idx)[i];
            }
        }
    }

    fn update_capture(
        &mut self,
        prev_acc: &Accumulator<IS_WHITE_POV>,
        sub_captured_idx: usize,
        add_piece_idx: usize,
        sub_piece_idx: usize,
    ) {
        unsafe {
            for i in 0..HL_SIZE {
                self.0[i] = prev_acc.0[i] - NET.ft_w.get_unchecked(sub_captured_idx)[i]
                    + NET.ft_w.get_unchecked(add_piece_idx)[i]
                    - NET.ft_w.get_unchecked(sub_piece_idx)[i];
            }
        }
    }

    fn update_non_capture(
        &mut self,
        prev_acc: &Accumulator<IS_WHITE_POV>,
        sub_piece_idx: usize,
        add_piece_idx: usize,
    ) {
        unsafe {
            for i in 0..HL_SIZE {
                self.0[i] = prev_acc.0[i] - NET.ft_w.get_unchecked(sub_piece_idx)[i]
                    + NET.ft_w.get_unchecked(add_piece_idx)[i];
            }
        }
    }
}

impl<const IS_WHITE_POV: bool> From<&Position> for Accumulator<{ IS_WHITE_POV }> {
    fn from(pos: &Position) -> Accumulator<{ IS_WHITE_POV }> {
        let mut acc = Accumulator(NET.hl_b);
        let king_sq: Square = pos.king_square(acc.pov());

        let mut enable_feature = |piece_color: Color, pt: PieceType, sq: Square| {
            let feature_idx: usize =
                acc.get_feature_idx(pos.in_check(), king_sq, piece_color, pt, sq);

            let ft_weights: &[i16; HL_SIZE] = unsafe { NET.ft_w.get_unchecked(feature_idx) };

            for (x, w) in acc.0.iter_mut().zip(ft_weights) {
                *x += *w;
            }
        };

        for piece_color in [Color::White, Color::Black] {
            for pt in PieceType::iter() {
                for square in pos.piece_bb(piece_color, pt) {
                    enable_feature(piece_color, pt, square);
                }
            }
        }

        acc
    }
}
