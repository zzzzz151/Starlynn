use std::fmt;
use crate::types::{Square, Rank, PieceType};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ChessMove {
    mov: u16
}

impl ChessMove {
    pub fn new(from: Square, to: Square, piece_type: PieceType) -> Self
    {
        // No pawns in backranks
        // No promotions in this constructor
        debug_assert!({
            let from_or_to_backrank = from.rank().is_backrank() || to.rank().is_backrank();
            !(piece_type == PieceType::Pawn && from_or_to_backrank)
        });

        let mut mov = Self::set_from_to(from, to);
        mov.mov |= piece_type as u16;
        mov
    }

    pub fn promotion(from: Square, to: Square, promotion: PieceType) -> Self
    {
        debug_assert!(from.rank() >= Rank::Rank7 || from.rank() <= Rank::Rank2);
        debug_assert!(promotion != PieceType::Pawn && promotion != PieceType::King);

        let mut mov = Self::set_from_to(from, to);
        mov.mov |= promotion as u16 + 10;
        mov
    }

    const fn set_from_to(from: Square, to: Square) -> Self
    {
        let mut mov = Self { mov: 0 };
        mov.mov |= (from as u16) << 10;
        mov.mov |= (to as u16) << 4;
        mov
    }

    pub const fn from(self) -> Square
    {
        let square_idx: u16 = (self.mov >> 10) & 0b111111;
        unsafe { std::mem::transmute(square_idx as u8) }
    }

    pub const fn to(self) -> Square
    {
        let square_idx: u16 = (self.mov >> 4) & 0b111111;
        unsafe { std::mem::transmute(square_idx as u8) }
    }

    pub const fn piece_type(self) -> PieceType
    {
        let right_4_bits = (self.mov & 0x000f) as u8;

        if right_4_bits <= (PieceType::King as u8)
        {
            // This move is not a promotion
            unsafe { std::mem::transmute(right_4_bits as u8) }
        }
        else {
            // This move is a promotion
            PieceType::Pawn
        }
    }

    pub fn promo_piece_type(self) -> Option<PieceType>
    {
        let right_4_bits = (self.mov & 0x000f) as u8;

        if right_4_bits <= (PieceType::King as u8)
        {
            // This move is not a promotion
            None
        }
        else {
            // This move is a promotion
            Some(PieceType::from_repr(right_4_bits - 10)).unwrap()
        }
    }

    pub fn is_castling(self) -> bool {
        self.piece_type() == PieceType::King && self.from().abs_diff(self.to()) == 2
    }
}

impl fmt::Display for ChessMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        if let Some(promotion) = self.promo_piece_type()
        {
            let str_promo = promotion.to_string().to_lowercase();
            write!(f, "{}{}{}", self.from(), self.to(), str_promo)
        }
        else {
            write!(f, "{}{}", self.from(), self.to())
        }

    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normal_move() {
        // Knight move
        let mut mov = ChessMove::new(Square::B1, Square::C3, PieceType::Knight);
        assert_eq!(mov.from(), Square::B1);
        assert_eq!(mov.to(), Square::C3);
        assert_eq!(mov.piece_type(), PieceType::Knight);
        assert_eq!(mov.promo_piece_type(), None);
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "b1c3");

        // Pawn move
        mov = ChessMove::new(Square::E2, Square::E4, PieceType::Pawn);
        assert_eq!(mov.piece_type(), PieceType::Pawn);
        assert_eq!(mov.promo_piece_type(), None);
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "e2e4");
    }

    #[test]
    fn test_promotion() {
        let mut mov = ChessMove::promotion(Square::A7, Square::A8, PieceType::Queen);
        assert_eq!(mov.piece_type(), PieceType::Pawn);
        assert_eq!(mov.promo_piece_type(), Some(PieceType::Queen));
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "a7a8q");

        mov = ChessMove::promotion(Square::A7, Square::A8, PieceType::Knight);
        assert_eq!(mov.promo_piece_type(), Some(PieceType::Knight));
        assert!(mov.to_string() == "a7a8n");
    }

    #[test]
    fn test_castling() {
        // Non-castling king move
        let mut mov = ChessMove::new(Square::E1, Square::F1, PieceType::King);
        assert_eq!(mov.piece_type(), PieceType::King);
        assert_eq!(mov.promo_piece_type(), None);
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "e1f1");

        // King side Castling
        mov = ChessMove::new(Square::E1, Square::G1, PieceType::King);
        assert_eq!(mov.piece_type(), PieceType::King);
        assert_eq!(mov.promo_piece_type(), None);
        assert!(mov.is_castling());
        assert!(mov.to_string() == "e1g1");

        // Queen side Castling
        mov = ChessMove::new(Square::E8, Square::C8, PieceType::King);
        assert_eq!(mov.piece_type(), PieceType::King);
        assert_eq!(mov.promo_piece_type(), None);
        assert!(mov.is_castling());
        assert!(mov.to_string() == "e8c8");
    }
}
