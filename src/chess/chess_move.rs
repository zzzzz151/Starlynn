use super::types::*;
use std::fmt;
use std::mem::{size_of, transmute};
use std::num::NonZeroU16;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ChessMove(NonZeroU16);

const _: () = assert!(size_of::<Option<ChessMove>>() == size_of::<u16>());

impl ChessMove {
    pub fn new(src: Square, dst: Square, pt: PieceType) -> Self {
        debug_assert!(src != dst);

        let mut mov: u16 = src as u16;
        mov |= (dst as u16) << 6;
        mov |= (pt as u16) << 12;

        debug_assert!(mov > 0);
        ChessMove::from(unsafe { NonZeroU16::new_unchecked(mov) })
    }

    pub fn new_promotion(src: Square, dst: Square, promo_pt: PieceType) -> Self {
        #[rustfmt::skip]
        debug_assert!(
            (src.rank() == Rank::Rank7 && dst.rank() == Rank::Rank8) ||
            (src.rank() == Rank::Rank2 && dst.rank() == Rank::Rank1)
        );

        debug_assert!(promo_pt != PieceType::Pawn && promo_pt != PieceType::King);

        let mut mov = ChessMove::new(src, dst, promo_pt);
        mov.0 |= 1 << 15; // Set promotion bit
        mov
    }

    pub fn from_uci(uci_move: &str, pt_moving: PieceType) -> Result<Self, String> {
        let uci_move = uci_move.trim();

        if uci_move.len() != 4 && uci_move.len() != 5 {
            return Err("UCI move must have exactly 4 or 5 characters".to_string());
        }

        let src: Square = Square::try_from(&uci_move[0..2])
            .map_err(|_| "Error parsing UCI move source square".to_string())?;

        let dst: Square = Square::try_from(&uci_move[2..4])
            .map_err(|_| "Error parsing UCI move target square".to_string())?;

        // Promotion
        if pt_moving == PieceType::Pawn && dst.rank().is_backrank() {
            let promo_pt = if uci_move.len() == 4 {
                PieceType::Queen
            } else {
                PieceType::try_from(uci_move.chars().last().unwrap())
                    .map_err(|_| "Error parsing promotion piece type in UCI move".to_string())?
            };

            return Ok(ChessMove::new_promotion(src, dst, promo_pt));
        }

        // Non-promotion
        if uci_move.len() != 4 {
            return Err("UCI move has 5 characters but is not a promotion".to_string());
        }

        Ok(ChessMove::new(src, dst, pt_moving))
    }

    pub const fn src(self) -> Square {
        let sq_idx: u16 = self.0.get() & 0b111_111;
        unsafe { transmute(sq_idx as u8) }
    }

    pub const fn dst(self) -> Square {
        let sq_idx: u16 = (self.0.get() >> 6) & 0b111_111;
        unsafe { transmute(sq_idx as u8) }
    }

    pub const fn piece_type(self) -> PieceType {
        if self.is_promotion() {
            PieceType::Pawn
        } else {
            let pt: u8 = (self.0.get() >> 12) as u8;
            debug_assert!(pt <= (PieceType::King as u8));
            unsafe { transmute(pt) }
        }
    }

    pub const fn is_promotion(self) -> bool {
        // Check promotion bit
        (self.0.get() >> 15) > 0
    }

    pub const fn promotion(self) -> Option<PieceType> {
        if self.is_promotion() {
            let mut promo_pt: u8 = (self.0.get() >> 12) as u8;
            promo_pt &= 0b111; // promo_pt has promotion bit set, so unset it

            #[rustfmt::skip]
            debug_assert!(
                promo_pt >= (PieceType::Knight as u8) &&
                promo_pt <= (PieceType::Queen as u8)
            );

            unsafe { transmute(promo_pt) }
        } else {
            None
        }
    }

    pub fn is_castling(self) -> bool {
        self.piece_type() == PieceType::King && self.src().abs_diff(self.dst()) == 2
    }
}

impl From<NonZeroU16> for ChessMove {
    fn from(mov: NonZeroU16) -> Self {
        ChessMove(mov)
    }
}

impl From<ChessMove> for NonZeroU16 {
    fn from(mov: ChessMove) -> Self {
        mov.0
    }
}

impl fmt::Display for ChessMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(promo_pt) = self.promotion() {
            write!(
                f,
                "{}{}{}",
                self.src(),
                self.dst(),
                piece_to_char(promo_pt, Color::Black)
            )
        } else {
            write!(f, "{}{}", self.src(), self.dst())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normal_move() {
        let mut mov = ChessMove::new(Square::E2, Square::E4, PieceType::Pawn);
        assert_eq!(mov.src(), Square::E2);
        assert_eq!(mov.dst(), Square::E4);
        assert_eq!(mov.piece_type(), PieceType::Pawn);
        assert_eq!(mov.promotion(), None);
        assert!(!mov.is_castling());
        assert_eq!(mov.to_string(), "e2e4");

        mov = ChessMove::new(Square::E1, Square::F1, PieceType::King);
        assert_eq!(mov.src(), Square::E1);
        assert_eq!(mov.dst(), Square::F1);
        assert_eq!(mov.piece_type(), PieceType::King);
        assert_eq!(mov.promotion(), None);
        assert!(!mov.is_castling());
        assert_eq!(mov.to_string(), "e1f1");
    }

    #[test]
    fn test_promotion() {
        let mut mov = ChessMove::new_promotion(Square::A7, Square::A8, PieceType::Queen);
        assert_eq!(mov.piece_type(), PieceType::Pawn);
        assert_eq!(mov.promotion(), Some(PieceType::Queen));
        assert!(!mov.is_castling());
        assert_eq!(mov.to_string(), "a7a8q");

        mov = ChessMove::new_promotion(Square::D2, Square::D1, PieceType::Knight);
        assert_eq!(mov.piece_type(), PieceType::Pawn);
        assert_eq!(mov.promotion(), Some(PieceType::Knight));
        assert!(!mov.is_castling());
        assert_eq!(mov.to_string(), "d2d1n");
    }

    #[test]
    fn test_castling() {
        // King side Castling
        let mut mov = ChessMove::new(Square::E1, Square::G1, PieceType::King);
        assert_eq!(mov.piece_type(), PieceType::King);
        assert_eq!(mov.promotion(), None);
        assert!(mov.is_castling());
        assert_eq!(mov.to_string(), "e1g1");

        // Queen side Castling
        mov = ChessMove::new(Square::E8, Square::C8, PieceType::King);
        assert_eq!(mov.piece_type(), PieceType::King);
        assert_eq!(mov.promotion(), None);
        assert!(mov.is_castling());
        assert_eq!(mov.to_string(), "e8c8");
    }

    #[test]
    fn test_move_from_uci() {
        // Normal move
        assert_eq!(
            ChessMove::from_uci("e2e4", PieceType::Pawn),
            Ok(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn))
        );

        // Knight promotion
        assert_eq!(
            ChessMove::from_uci("a7a8n", PieceType::Pawn),
            Ok(ChessMove::new_promotion(
                Square::A7,
                Square::A8,
                PieceType::Knight
            ))
        );

        // Default to queen promotion
        assert_eq!(
            ChessMove::from_uci("a2a1", PieceType::Pawn),
            Ok(ChessMove::new_promotion(
                Square::A2,
                Square::A1,
                PieceType::Queen
            ))
        );
    }
}
