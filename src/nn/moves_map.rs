use crate::chess::{attacks::*, bitboard::Bitboard, chess_move::ChessMove, types::*};
use std::mem::transmute;
use strum::IntoEnumIterator;

// [src][dst][promo_piece_type else 6]
static MOVES_MAP_1880: [[[i16; 7]; 64]; 64] =
    unsafe { transmute(*include_bytes!("../embeds/moves_map_1880.bin")) };

// [src][dst][promo_piece_type else 6]
pub fn map_moves_1880() -> [[[i16; 7]; 64]; 64] {
    let mut result = [[[-1_i16; 7]; 64]; 64];
    let mut count: usize = 0;

    // Non-promotions
    for src_sq in Square::iter() {
        let attacks: Bitboard = KNIGHT_ATTACKS[src_sq]
            | BISHOP_ATTACKS[src_sq].attacks(Bitboard::from(0))
            | ROOK_ATTACKS[src_sq].attacks(Bitboard::from(0));

        for dst_sq in attacks {
            result[src_sq][dst_sq][6] = count as i16;
            count += 1;
        }
    }

    // Promotions
    for src_sq in Bitboard::from(Rank::Rank7) {
        let single_push_sq: Square = unsafe { transmute(src_sq as u8 + 8) };

        for dst_sq in Bitboard::from(single_push_sq) | PAWN_ATTACKS[Color::White][src_sq] {
            for promo_pt in [
                PieceType::Knight,
                PieceType::Bishop,
                PieceType::Rook,
                PieceType::Queen,
            ] {
                result[src_sq][dst_sq][promo_pt as usize] = count as i16;
                count += 1;
            }
        }
    }

    assert_eq!(count, 1880);
    result
}

pub fn get_move_idx_1882(mov: ChessMove) -> usize {
    if mov.is_castling() {
        return 1880 + ((mov.dst() > mov.src()) as usize);
    }

    let promo_pt_idx: usize = if let Some(promo_piece_type) = mov.promotion() {
        promo_piece_type as usize
    } else {
        6
    };

    let idx_1880: i16 =
        unsafe { *MOVES_MAP_1880[mov.src()][mov.dst()].get_unchecked(promo_pt_idx) };

    debug_assert!((0..1880).contains(&idx_1880));
    idx_1880 as usize
}
