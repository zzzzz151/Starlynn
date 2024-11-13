use strum::FromRepr;
use strum_macros::EnumIter;
use std::fmt;
use std::ops::{Not, Index, IndexMut};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, FromRepr, EnumIter)]
#[repr(u8)]
pub enum Color {
    White, Black
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, FromRepr, EnumIter)]
#[repr(u8)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, FromRepr, EnumIter)]
#[repr(u8)]
pub enum File {
    A, B, C, D, E, F, G, H
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, FromRepr, EnumIter)]
#[repr(u8)]
pub enum Rank {
    Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, FromRepr, EnumIter)]
#[repr(u8)]
pub enum PieceType {
    Pawn, Knight, Bishop, Rook, Queen, King
}

impl Not for Color
{
    type Output = Self;

    fn not(self) -> Self::Output
    {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White
        }
    }
}

impl fmt::Display for Color
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{}", if *self == Color::White { "w" } else { "b" })
    }
}

impl TryFrom<&str> for Color
{
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error>
    {
        match s.to_lowercase().as_str() {
            "w" => Ok(Color::White),
            "b" => Ok(Color::Black),
            _ => Err(()),
        }
    }
}

impl<T> Index<Color> for [T; 2]
{
    type Output = T;

    fn index(&self, color: Color) -> &Self::Output
    {
        unsafe { self.get_unchecked(color as usize) }
    }
}

impl<T> IndexMut<Color> for [T; 2]
{
    fn index_mut(&mut self, color: Color) -> &mut Self::Output
    {
        unsafe { self.get_unchecked_mut(color as usize) }
    }
}

impl Square {
    pub const fn from_file_rank(file: File, rank: Rank) -> Self
    {
        let square_idx = (rank as u8) * 8 + (file as u8);
        unsafe { std::mem::transmute(square_idx) }
    }

    pub const fn file(self) -> File {
        unsafe { std::mem::transmute(self as u8 % 8) }
    }

    pub const fn rank(self) -> Rank {
        unsafe { std::mem::transmute(self as u8 / 8) }
    }

    pub const fn file_flipped(self) -> Square {
        unsafe { std::mem::transmute(self as u8 ^ 7) }
    }

    pub const fn rank_flipped(self) -> Square {
        unsafe { std::mem::transmute(self as u8 ^ 56) }
    }

    pub const fn abs_diff(self, other: Square) -> u8
    {
        let diff = self as i8 - (other as i8);
        diff.abs() as u8
    }
}

impl fmt::Display for Square
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let file = 'a' as u8 + (self.file() as u8);
        write!(f, "{}{}", file as char, self.rank() as u8 + 1)
    }
}

impl TryFrom<&str> for Square {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error>
    {
        if s.len() != 2 { return Err(()); }

        let mut chars = s.chars();
        let file_char = chars.next().unwrap().to_ascii_lowercase() as u8;
        let rank_char = chars.next().unwrap().to_ascii_lowercase() as u8;

        if file_char < ('a' as u8) || file_char > ('h' as u8)
        || rank_char < ('1' as u8) || rank_char > ('8' as u8)
        {
            return Err(());
        }

        let file_idx = file_char - ('a' as u8);
        let rank_idx = rank_char - ('1' as u8);

        Ok(Square::from_file_rank(
            File::from_repr(file_idx).unwrap(),
            Rank::from_repr(rank_idx).unwrap()
        ))
    }
}

impl<T> Index<Square> for [T; 64]
{
    type Output = T;

    fn index(&self, square: Square) -> &Self::Output
    {
        unsafe { self.get_unchecked(square as usize) }
    }
}

impl<T> IndexMut<Square> for [T; 64]
{
    fn index_mut(&mut self, square: Square) -> &mut Self::Output
    {
        unsafe { self.get_unchecked_mut(square as usize) }
    }
}

impl<T> Index<File> for [T; 8]
{
    type Output = T;

    fn index(&self, file: File) -> &Self::Output
    {
        unsafe { self.get_unchecked(file as usize) }
    }
}

impl<T> IndexMut<File> for [T; 8]
{
    fn index_mut(&mut self, file: File) -> &mut Self::Output
    {
        unsafe { self.get_unchecked_mut(file as usize) }
    }
}

impl Rank {
    pub fn is_backrank(self) -> bool {
        self == Rank::Rank1 || self == Rank::Rank8
    }
}

impl<T> Index<Rank> for [T; 8]
{
    type Output = T;

    fn index(&self, rank: Rank) -> &Self::Output
    {
        unsafe { self.get_unchecked(rank as usize) }
    }
}

impl<T> IndexMut<Rank> for [T; 8]
{
    fn index_mut(&mut self, rank: Rank) -> &mut Self::Output
    {
        unsafe { self.get_unchecked_mut(rank as usize) }
    }
}

impl fmt::Display for PieceType
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let chr = match self {
            PieceType::Pawn   => 'P',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Rook   => 'R',
            PieceType::Queen  => 'Q',
            PieceType::King   => 'K'
        };

        write!(f, "{}", chr)
    }
}

impl TryFrom<&str> for PieceType
{
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error>
    {
        match s.to_uppercase().as_str()
        {
            "P" => Ok(PieceType::Pawn),
            "N" => Ok(PieceType::Knight),
            "B" => Ok(PieceType::Bishop),
            "R" => Ok(PieceType::Rook),
            "Q" => Ok(PieceType::Queen),
            "K" => Ok(PieceType::King),
            _ => Err(())
        }
    }
}

impl<T> Index<PieceType> for [T; 6]
{
    type Output = T;

    fn index(&self, pt: PieceType) -> &Self::Output
    {
        unsafe { self.get_unchecked(pt as usize) }
    }
}

impl<T> IndexMut<PieceType> for [T; 6]
{
    fn index_mut(&mut self, pt: PieceType) -> &mut Self::Output
    {
        unsafe { self.get_unchecked_mut(pt as usize) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_square()
    {
        assert_eq!(Square::B1.file(), File::B);
        assert_eq!(Square::B1.rank(), Rank::Rank1);
        assert_eq!(Square::B1.to_string(), "b1");

        let sq = Square::from_file_rank(File::B, Rank::Rank5);
        assert_eq!(sq as u8, 33);

        assert_eq!(sq.file(), File::B);
        assert_eq!(sq.rank(), Rank::Rank5);

        let sq_flipped = sq.file_flipped().rank_flipped();
        assert_eq!(sq_flipped, Square::from_file_rank(File::G, Rank::Rank4));

        assert_eq!(format!("{}", sq), "b5");
        assert_eq!(sq_flipped.to_string(), "g4");

        assert_eq!(Square::try_from("b5").unwrap(), sq);
        assert_eq!(Square::try_from("g4").unwrap(), sq_flipped);
    }

    #[test]
    fn test_piece_type()
    {
        assert_eq!(PieceType::try_from("n").unwrap(), PieceType::Knight);
        assert_eq!(PieceType::try_from("N").unwrap(), PieceType::Knight);
    }
}
