use super::params::{FT_Q, HALF_HL_SIZE, NET};
use std::mem::transmute;
use strum::IntoEnumIterator;

use crate::chess::{
    chess_move::ChessMove,
    pos_state::PosState,
    position::Position,
    types::{Color, PieceType, Square},
};

#[repr(C, align(64))]
#[derive(Clone)]
pub struct BothAccumulators {
    unactivated_accs: [[i16; HALF_HL_SIZE]; 2],
    activated_accs: [[i16; HALF_HL_SIZE]; 2],
    pub is_unactivated_updated: bool,
    is_activated_updated: bool,
}

impl BothAccumulators {
    pub fn new() -> Self {
        BothAccumulators {
            unactivated_accs: [NET.hl_b, NET.hl_b],
            activated_accs: [[0; HALF_HL_SIZE]; 2],
            is_unactivated_updated: true,
            is_activated_updated: false,
        }
    }

    // SCReLU activation
    // clamp(x, 0.0, 1.0)^2
    // clamp(x, 0, FT_Q)^2
    // FT max weight/bias and FT_Q ensures the mul fits in i16
    pub fn activated_accs(&mut self) -> &[[i16; HALF_HL_SIZE]; 2] {
        assert!(self.is_unactivated_updated);

        if self.is_activated_updated {
            return &self.activated_accs;
        }

        for (act_acc, unact_acc) in self.activated_accs.iter_mut().zip(&self.unactivated_accs) {
            for i in 0..HALF_HL_SIZE {
                let x: i16 = unact_acc[i].clamp(0, FT_Q);
                debug_assert!((x as i32 * (x as i32)).abs() <= 32767);
                act_acc[i] = unsafe { x.unchecked_mul(x) };
            }
        }

        self.is_activated_updated = true;
        &self.activated_accs
    }

    pub fn enable_feature(&mut self, mut piece_color: Color, pt: PieceType, mut sq: Square) {
        for unact_acc in &mut self.unactivated_accs {
            for (i, x) in unact_acc.iter_mut().enumerate() {
                *x += NET.ft_w[piece_color][pt][sq][i];
            }

            piece_color = !piece_color;
            sq = sq.rank_flipped();
        }

        self.is_activated_updated = false;
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

        debug_assert!(pos_after_move.state::<1>().is_some());
        let state_moved: &PosState = unsafe { pos_after_move.state::<1>().unwrap_unchecked() };

        let stm: Color = state_moved.side_to_move();
        let mut piece_color = stm;

        debug_assert!(pos_after_move.last_move().is_some());
        let mov: ChessMove = unsafe { pos_after_move.last_move().unwrap_unchecked() };

        let mut src: Square = mov.src();
        let mut dst: Square = mov.dst();

        let pt_moved: PieceType = mov.piece_type();
        let pt_placed: PieceType = mov.promotion().unwrap_or(pt_moved);

        let mut captured_piece_sq: Square =
            if pt_moved == PieceType::Pawn && Some(dst) == state_moved.en_passant_square() {
                unsafe { transmute(dst as u8 ^ 8) }
            } else {
                dst
            };

        for (acc, prev_acc) in self
            .unactivated_accs
            .iter_mut()
            .zip(&prev_accs.unactivated_accs)
        {
            if mov.is_castling() {
                // Rook from to
                let (rook_src, rook_dst) = match dst {
                    Square::C1 => (Square::A1, Square::D1),
                    Square::G1 => (Square::H1, Square::F1),
                    Square::C8 => (Square::A8, Square::D8),
                    Square::G8 => (Square::H8, Square::F8),
                    _ => panic!("Invalid castling king target square"),
                };

                update_castling(acc, prev_acc, piece_color, src, dst, rook_src, rook_dst);
            } else if let Some(pt_captured) = pos_after_move.piece_type_captured() {
                update_capture(
                    acc,
                    prev_acc,
                    piece_color,
                    pt_moved,
                    pt_placed,
                    src,
                    dst,
                    pt_captured,
                    captured_piece_sq,
                );

                captured_piece_sq = captured_piece_sq.rank_flipped();
            } else {
                update_non_capture(acc, prev_acc, piece_color, pt_moved, pt_placed, src, dst);
            }

            piece_color = !piece_color;
            src = src.rank_flipped();
            dst = dst.rank_flipped();
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

        for piece_color in [Color::White, Color::Black] {
            for pt in PieceType::iter() {
                for square in pos.piece_bb(piece_color, pt) {
                    both_accs.enable_feature(piece_color, pt, square);
                }
            }
        }

        both_accs
    }
}

fn update_castling(
    acc: &mut [i16; HALF_HL_SIZE],
    prev_acc: &[i16; HALF_HL_SIZE],
    piece_color: Color,
    king_src: Square,
    king_dst: Square,
    rook_src: Square,
    rook_dst: Square,
) {
    for i in 0..HALF_HL_SIZE {
        acc[i] = prev_acc[i] - NET.ft_w[piece_color][PieceType::King][king_src][i]
            + NET.ft_w[piece_color][PieceType::King][king_dst][i]
            - NET.ft_w[piece_color][PieceType::Rook][rook_src][i]
            + NET.ft_w[piece_color][PieceType::Rook][rook_dst][i];
    }
}

#[allow(clippy::too_many_arguments)]
fn update_capture(
    acc: &mut [i16; HALF_HL_SIZE],
    prev_acc: &[i16; HALF_HL_SIZE],
    piece_color: Color,
    pt_moved: PieceType,
    pt_placed: PieceType,
    src: Square,
    dst: Square,
    pt_captured: PieceType,
    captured_piece_sq: Square,
) {
    for i in 0..HALF_HL_SIZE {
        acc[i] = prev_acc[i] - NET.ft_w[!piece_color][pt_captured][captured_piece_sq][i]
            + NET.ft_w[piece_color][pt_placed][dst][i]
            - NET.ft_w[piece_color][pt_moved][src][i];
    }
}

fn update_non_capture(
    acc: &mut [i16; HALF_HL_SIZE],
    prev_acc: &[i16; HALF_HL_SIZE],
    piece_color: Color,
    pt_moved: PieceType,
    pt_placed: PieceType,
    src: Square,
    dst: Square,
) {
    for i in 0..HALF_HL_SIZE {
        acc[i] = prev_acc[i] - NET.ft_w[piece_color][pt_moved][src][i]
            + NET.ft_w[piece_color][pt_placed][dst][i];
    }
}
