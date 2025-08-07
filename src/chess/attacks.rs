use super::bitboard::Bitboard;
use std::mem::transmute;

#[repr(C)]
pub struct MagicEntry<const ATTACKS_TABLE_SIZE: usize> {
    attacks_empty_board_excluding_last_sq_each_dir: Bitboard,
    magic: u64,
    shift: u64,
    attacks_by_key: [Bitboard; ATTACKS_TABLE_SIZE],
}

impl<const ATTACKS_TABLE_SIZE: usize> MagicEntry<ATTACKS_TABLE_SIZE> {
    pub fn attacks(&self, occ: Bitboard) -> Bitboard {
        let blockers: Bitboard = occ & self.attacks_empty_board_excluding_last_sq_each_dir;
        let idx = u64::from(blockers).wrapping_mul(self.magic) >> self.shift;

        debug_assert!(
            (idx as usize) < ATTACKS_TABLE_SIZE,
            "Magic index out of bounds"
        );

        unsafe { *(self.attacks_by_key.get_unchecked(idx as usize)) }
    }
}

// [color][square]
pub const PAWN_ATTACKS: [[Bitboard; 64]; 2] =
    unsafe { transmute(*include_bytes!("../embeds/pawn_attacks.bin")) };

// [square]
pub const KNIGHT_ATTACKS: [Bitboard; 64] =
    unsafe { transmute(*include_bytes!("../embeds/knight_attacks.bin")) };

// [square][key]
pub static BISHOP_ATTACKS: [MagicEntry<512>; 64] =
    unsafe { transmute(*include_bytes!("../embeds/bishop_attacks.bin")) };

// [square][key]
pub static ROOK_ATTACKS: [MagicEntry<4096>; 64] =
    unsafe { transmute(*include_bytes!("../embeds/rook_attacks.bin")) };

// [square]
pub const KING_ATTACKS: [Bitboard; 64] =
    unsafe { transmute(*include_bytes!("../embeds/king_attacks.bin")) };

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess::types::*;

    #[test]
    fn test_pawn_knight_king_attacks() {
        assert_eq!(
            PAWN_ATTACKS[Color::White][Square::E4],
            Bitboard::from([Square::D5, Square::F5].as_slice())
        );

        assert_eq!(
            PAWN_ATTACKS[Color::Black][Square::A2],
            Bitboard::from(Square::B1)
        );

        assert_eq!(KNIGHT_ATTACKS[Square::A3], Bitboard::from(8657044482));
        assert_eq!(KNIGHT_ATTACKS[Square::D4], Bitboard::from(22136263676928));

        assert_eq!(KING_ATTACKS[Square::B1], Bitboard::from(1797));
        assert_eq!(KING_ATTACKS[Square::D4], Bitboard::from(120596463616));
    }

    #[test]
    fn test_bishop_rook_attacks() {
        assert_eq!(
            BISHOP_ATTACKS[Square::B2].attacks(Bitboard::from(0)),
            Bitboard::from(9241421688590368773)
        );

        assert_eq!(
            BISHOP_ATTACKS[Square::B2].attacks(Bitboard::from(68719542784)),
            Bitboard::from(68854022149)
        );

        assert_eq!(
            ROOK_ATTACKS[Square::B1].attacks(Bitboard::from(0)),
            Bitboard::from(144680345676153597)
        );

        assert_eq!(
            ROOK_ATTACKS[Square::B1].attacks(Bitboard::from(562949953421442)),
            Bitboard::from(565157600297725)
        );
    }
}
