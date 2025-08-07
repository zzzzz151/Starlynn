use std::mem::transmute;

#[rustfmt::skip]
use derive_more::{
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign,
    Not, Shl, ShlAssign, Shr, ShrAssign
};

use super::types::*;

#[rustfmt::skip]
#[derive(
    Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd,
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign,
    Not, Shl, ShlAssign, Shr, ShrAssign
)]
pub struct Bitboard(u64);

impl Bitboard {
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn is_full(self) -> bool {
        self.0 == !0
    }

    pub const fn count(self) -> u32 {
        self.0.count_ones()
    }

    pub fn contains(self, sq: Square) -> bool {
        let and = self & Bitboard::from(sq);
        !and.is_empty()
    }

    pub const fn first_square(self) -> Option<Square> {
        if self.is_empty() {
            return None;
        }

        let sq_idx: u8 = self.0.trailing_zeros() as u8;
        unsafe { Some(transmute(sq_idx)) }
    }

    pub fn pop_square(&mut self) -> Option<Square> {
        let sq: Option<Square> = self.first_square();

        if sq.is_some() {
            *self &= Bitboard::from(self.0 - 1);
        }

        sq
    }
}

impl From<u64> for Bitboard {
    fn from(bitboard: u64) -> Self {
        Bitboard(bitboard)
    }
}

impl From<Bitboard> for u64 {
    fn from(bitboard: Bitboard) -> Self {
        bitboard.0
    }
}

impl From<Square> for Bitboard {
    fn from(sq: Square) -> Self {
        Bitboard::from(1) << (sq as u8)
    }
}

impl From<&[Square]> for Bitboard {
    fn from(squares: &[Square]) -> Self {
        let mut bb = Bitboard::from(0);

        for &sq in squares {
            bb |= Bitboard::from(sq);
        }

        bb
    }
}

impl From<File> for Bitboard {
    fn from(file: File) -> Self {
        const FILE_A_BB: Bitboard = unsafe { transmute(0x101010101010101u64) };
        FILE_A_BB << (file as u8)
    }
}

impl From<Rank> for Bitboard {
    fn from(rank: Rank) -> Self {
        const RANK_1_BB: Bitboard = unsafe { transmute(0xffu64) };
        RANK_1_BB << (rank as u8 * 8)
    }
}

impl Iterator for Bitboard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop_square()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitboard() {
        let bb = Bitboard::from([Square::A5, Square::D4].as_slice());
        assert_eq!(bb.count(), 2);

        assert!(bb.contains(Square::A5));
        assert!(bb.contains(Square::D4));

        let mut iter = bb.into_iter();
        assert_eq!(iter.next(), Some(Square::D4));
        assert_eq!(iter.next(), Some(Square::A5));
        assert_eq!(iter.next(), None);
    }
}
