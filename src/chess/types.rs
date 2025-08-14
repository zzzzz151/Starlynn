use std::mem::transmute;
use std::ops::{Index, IndexMut, Not};
use strum_macros::{Display, EnumIter, EnumString};

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Display, EnumString, EnumIter)]
#[strum(serialize_all = "lowercase", ascii_case_insensitive)]
#[rustfmt::skip]
pub enum Square {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, EnumIter)]
#[rustfmt::skip]
pub enum File {
    A, B, C, D, E, F, G, H
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, EnumIter)]
#[rustfmt::skip]
pub enum Rank {
    Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[rustfmt::skip]
pub enum Color {
    White, Black
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, EnumIter)]
#[rustfmt::skip]
pub enum PieceType {
    Pawn, Knight, Bishop, Rook, Queen, King
}

impl Square {
    pub const fn new(file: File, rank: Rank) -> Self {
        unsafe { transmute(file as u8 + (rank as u8) * 8) }
    }

    pub const fn file(self) -> File {
        unsafe { transmute(self as u8 % 8) }
    }

    pub const fn rank(self) -> Rank {
        unsafe { transmute(self as u8 / 8) }
    }

    pub const fn file_flipped(self) -> Square {
        unsafe { transmute(self as u8 ^ 7) }
    }

    pub const fn rank_flipped(self) -> Square {
        unsafe { transmute(self as u8 ^ 56) }
    }

    pub const fn abs_diff(self, other: Square) -> u32 {
        let diff: i32 = self as i32 - (other as i32);
        diff.unsigned_abs()
    }
}

impl<T> Index<Square> for [T; 64] {
    type Output = T;

    fn index(&self, sq: Square) -> &Self::Output {
        unsafe { self.get_unchecked(sq as usize) }
    }
}

impl<T> IndexMut<Square> for [T; 64] {
    fn index_mut(&mut self, sq: Square) -> &mut Self::Output {
        unsafe { self.get_unchecked_mut(sq as usize) }
    }
}

impl<T> Index<File> for [T; 8] {
    type Output = T;

    fn index(&self, file: File) -> &Self::Output {
        unsafe { self.get_unchecked(file as usize) }
    }
}

impl<T> IndexMut<File> for [T; 8] {
    fn index_mut(&mut self, file: File) -> &mut Self::Output {
        unsafe { self.get_unchecked_mut(file as usize) }
    }
}

impl Rank {
    pub fn is_backrank(self) -> bool {
        self == Rank::Rank1 || self == Rank::Rank8
    }
}

impl<T> Index<Rank> for [T; 8] {
    type Output = T;

    fn index(&self, rank: Rank) -> &Self::Output {
        unsafe { self.get_unchecked(rank as usize) }
    }
}

impl<T> IndexMut<Rank> for [T; 8] {
    fn index_mut(&mut self, rank: Rank) -> &mut Self::Output {
        unsafe { self.get_unchecked_mut(rank as usize) }
    }
}

impl Not for Color {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

impl<T> Index<Color> for [T; 2] {
    type Output = T;

    fn index(&self, color: Color) -> &Self::Output {
        unsafe { self.get_unchecked(color as usize) }
    }
}

impl<T> IndexMut<Color> for [T; 2] {
    fn index_mut(&mut self, color: Color) -> &mut Self::Output {
        unsafe { self.get_unchecked_mut(color as usize) }
    }
}

impl<T> Index<PieceType> for [T; 6] {
    type Output = T;

    fn index(&self, pt: PieceType) -> &Self::Output {
        unsafe { self.get_unchecked(pt as usize) }
    }
}

impl<T> IndexMut<PieceType> for [T; 6] {
    fn index_mut(&mut self, pt: PieceType) -> &mut Self::Output {
        unsafe { self.get_unchecked_mut(pt as usize) }
    }
}

impl TryFrom<char> for PieceType {
    type Error = ();

    fn try_from(chr: char) -> Result<Self, Self::Error> {
        match chr.to_ascii_uppercase() {
            'P' => Ok(PieceType::Pawn),
            'N' => Ok(PieceType::Knight),
            'B' => Ok(PieceType::Bishop),
            'R' => Ok(PieceType::Rook),
            'Q' => Ok(PieceType::Queen),
            'K' => Ok(PieceType::King),
            _ => Err(()),
        }
    }
}

pub fn piece_to_char(pt: PieceType, color: Color) -> char {
    let char_upper: char = ['P', 'N', 'B', 'R', 'Q', 'K'][pt];

    match color {
        Color::White => char_upper,
        Color::Black => char_upper.to_ascii_lowercase(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_square() {
        assert_eq!(Square::D4.to_string(), "d4");
        assert_eq!(Square::try_from("d4").unwrap(), Square::D4);
        assert_eq!(Square::try_from("D4").unwrap(), Square::D4);
        assert!(Square::try_from("d45").is_err());
        assert!(Square::try_from("a9").is_err());
        assert!(Square::try_from("i1").is_err());

        assert_eq!(Square::new(File::D, Rank::Rank4), Square::D4);
        assert_eq!(Square::D4.file(), File::D);
        assert_eq!(Square::D4.rank(), Rank::Rank4);
        assert_eq!(Square::C3.file_flipped().rank_flipped(), Square::F6);
    }

    #[test]
    fn test_piece_type() {
        assert_eq!(PieceType::try_from('N').unwrap(), PieceType::Knight);
        assert_eq!(PieceType::try_from('n').unwrap(), PieceType::Knight);

        assert_eq!(piece_to_char(PieceType::Knight, Color::White), 'N');
        assert_eq!(piece_to_char(PieceType::Knight, Color::Black), 'n');
    }
}
