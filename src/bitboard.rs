use std::ops::{
    Not, BitAnd, BitAndAssign, BitOr, BitOrAssign,
    BitXor, BitXorAssign, Shl, Shr, Mul
};

use crate::types::Square;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Bitboard {
    bitboard: u64
}

impl Bitboard {
    pub const EMPTY: Self = Self { bitboard: 0 };
    pub const FULL: Self = Self { bitboard: !0 };

    // [rank]
    pub const RANK: [Self; 8] = [
        Self { bitboard: 0x0000_0000_0000_00FF },
        Self { bitboard: 0x0000_0000_0000_FF00 },
        Self { bitboard: 0x0000_0000_00FF_0000 },
        Self { bitboard: 0x0000_0000_FF00_0000 },
        Self { bitboard: 0x0000_00FF_0000_0000 },
        Self { bitboard: 0x0000_FF00_0000_0000 },
        Self { bitboard: 0x00FF_0000_0000_0000 },
        Self { bitboard: 0xFF00_0000_0000_0000 }
    ];

    // [file]
    pub const FILE: [Self; 8] = [
        Self { bitboard: 0x0101_0101_0101_0101 },
        Self { bitboard: 0x0202_0202_0202_0202 },
        Self { bitboard: 0x0404_0404_0404_0404 },
        Self { bitboard: 0x0808_0808_0808_0808 },
        Self { bitboard: 0x1010_1010_1010_1010 },
        Self { bitboard: 0x2020_2020_2020_2020 },
        Self { bitboard: 0x4040_4040_4040_4040 },
        Self { bitboard: 0x8080_8080_8080_8080 }
    ];

    pub const fn count(self) -> u32 {
        self.bitboard.count_ones()
    }

    pub fn contains_square(self, square: Square) -> bool {
        (self & Self::from(square)) != Bitboard::EMPTY
    }

    pub fn first_square(self) -> Option<Square>
    {
        if self == Bitboard::EMPTY { return None; }

        let square_idx = self.bitboard.trailing_zeros() as u8;
        unsafe { Some(std::mem::transmute(square_idx)) }
    }
}

impl From<u64> for Bitboard {
    fn from(bitboard: u64) -> Self {
        Self { bitboard: bitboard }
    }
}

impl From<Bitboard> for u64 {
    fn from(bitboard: Bitboard) -> Self {
        bitboard.bitboard
    }
}

impl From<Square> for Bitboard {
    fn from(square: Square) -> Self {
        Bitboard { bitboard: 1u64 << (square as u8) }
    }
}

impl Iterator for Bitboard
{
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item>
    {
        let square: Option<Square> = self.first_square();

        if square != None {
            self.bitboard &= self.bitboard - 1;
        }

        square
    }
}

impl Not for Bitboard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self { bitboard: !self.bitboard }
    }
}

impl BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self { bitboard: self.bitboard & rhs.bitboard }
    }
}

impl BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.bitboard &= rhs.bitboard;
    }
}

impl BitOr for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self { bitboard: self.bitboard | rhs.bitboard }
    }
}

impl BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.bitboard |= rhs.bitboard;
    }
}

impl BitXor for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self { bitboard: self.bitboard ^ rhs.bitboard }
    }
}

impl BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.bitboard ^= rhs.bitboard;
    }
}

impl Shr<u8> for Bitboard {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        Self { bitboard: self.bitboard >> rhs }
    }
}

impl Shl<u8> for Bitboard {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        Self { bitboard: self.bitboard << rhs }
    }
}

impl Mul for Bitboard {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self { bitboard: self.bitboard * rhs.bitboard }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitboard() {
        let mut bb = Bitboard::from(Square::A5);
        assert_eq!(bb.first_square().unwrap(), Square::A5);
        assert_eq!(bb.count(), 1);

        bb |= Bitboard::from(Square::D4);
        assert!(bb.contains_square(Square::A5));
        assert!(bb.contains_square(Square::D4));
        assert_eq!(bb.count(), 2);

        let mut iter = bb.into_iter();
        assert_eq!(iter.next().unwrap(), Square::D4);
        assert_eq!(iter.next().unwrap(), Square::A5);
        assert_eq!(iter.next(), None);
    }
}
