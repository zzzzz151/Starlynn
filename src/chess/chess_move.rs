use std::num::NonZeroU16;
use std::fmt;
use std::mem::transmute;
use super::types::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ChessMove(NonZeroU16);

const _: () = assert!(std::mem::size_of::<Option<ChessMove>>() == std::mem::size_of::<u16>());

impl ChessMove
{
    pub fn new(src: Square, dst: Square, pt: PieceType) -> Self
    {
        debug_assert!(pt != PieceType::Pawn ||
            (!src.rank().is_backrank() && !dst.rank().is_backrank()));

        ChessMove::new_internal(src, dst, pt as u16)
    }

    pub fn new_promotion(src: Square, dst: Square, promo_pt: PieceType) -> Self
    {
        debug_assert!(
            (src.rank() == Rank::Rank7 && dst.rank() == Rank::Rank8) ||
            (src.rank() == Rank::Rank2 && dst.rank() == Rank::Rank1)
        );

        debug_assert!(promo_pt != PieceType::Pawn && promo_pt != PieceType::King);

        ChessMove::new_internal(src, dst, promo_pt as u16 + 10)
    }

    fn new_internal(src: Square, dst: Square, flag: u16) -> Self
    {
        debug_assert!(src != dst);

        let mut mov: u16 = (src as u16) << 10;
        mov |= (dst as u16) << 4;
        mov |= flag;

        debug_assert!(mov > 0);
        ChessMove::from(unsafe { NonZeroU16::new_unchecked(mov) })
    }

    pub fn from_uci(uci_move: &str, pt_moving: PieceType) -> Result<Self, String>
    {
        let uci_move = uci_move.trim();

        if uci_move.len() != 4 && uci_move.len() != 5
        {
            return Err("UCI move must have exactly 4 or 5 characters".to_string());
        }

        let src = Square::try_from(&uci_move[0..2])
            .map_err(|_| "Error parsing UCI move source square".to_string())?;

        let dst = Square::try_from(&uci_move[2..4])
            .map_err(|_| "Error parsing UCI move target square".to_string())?;

        // Promotion
        if pt_moving == PieceType::Pawn && dst.rank().is_backrank()
        {
            let promo_pt = if uci_move.len() == 4 {
                PieceType::Queen
            } else {
                PieceType::try_from(&uci_move[4..5])
                    .map_err(|_| "Invalid promotion piece type in UCI move".to_string())?
            };

            return Ok(ChessMove::new_promotion(src, dst, promo_pt));
        }

        // Non-promotion
        if uci_move.len() != 4
        {
            return Err("UCI move has 5 characters but is not a promotion".to_string());
        }

        Ok(ChessMove::new(src, dst, pt_moving))
    }

    pub const fn src(self) -> Square
    {
        let sq_idx: u16 = self.0.get() >> 10;
        unsafe { transmute(sq_idx as u8) }
    }

    pub const fn dst(self) -> Square
    {
        let sq_idx: u16 = (self.0.get() >> 4) & 0b111111;
        unsafe { transmute(sq_idx as u8) }
    }

    pub const fn piece_type(self) -> PieceType
    {
        let low_4_bits = (self.0.get() & 0x000f) as u8;

        // Promotion?
        if low_4_bits > (PieceType::King as u8)
        {
            return PieceType::Pawn;
        }

        unsafe { transmute(low_4_bits) }
    }

    pub const fn promotion(self) -> Option<PieceType>
    {
        let low_4_bits = (self.0.get() & 0x000f) as u8;

        // If not a promotion, return None
        if low_4_bits <= (PieceType::King as u8)
        {
            return None;
        }

        debug_assert!(low_4_bits >= 11 && low_4_bits <= 14);
        unsafe { transmute(low_4_bits - 10) }
    }

    pub fn is_castling(self) -> bool
    {
        self.piece_type() == PieceType::King && self.src().abs_diff(self.dst()) == 2
    }

}

impl From<NonZeroU16> for ChessMove
{
    fn from(mov: NonZeroU16) -> Self { ChessMove(mov) }
}

impl From<ChessMove> for NonZeroU16
{
    fn from(mov: ChessMove) -> Self { mov.0 }
}

impl fmt::Display for ChessMove
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        if let Some(promo_pt) = self.promotion()
        {
            write!(f, "{}{}{}", self.src(), self.dst(), promo_pt.to_string().to_lowercase())
        }
        else {
            write!(f, "{}{}", self.src(), self.dst())
        }

    }
}

#[cfg(test)]
mod tests
{
    use super::*;

    #[test]
    fn test_normal_move()
    {
        let mut mov = ChessMove::new(Square::E2, Square::E4, PieceType::Pawn);
        assert!(mov.src() == Square::E2);
        assert!(mov.dst() == Square::E4);
        assert!(mov.piece_type() == PieceType::Pawn);
        assert!(mov.promotion().is_none());
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "e2e4");

        mov = ChessMove::new(Square::E1, Square::G1, PieceType::Queen);
        assert!(mov.src() == Square::E1);
        assert!(mov.dst() == Square::G1);
        assert!(mov.piece_type() == PieceType::Queen);
        assert!(mov.promotion().is_none());
        assert!(!mov.is_castling());
        assert!(format!("{}", mov) == "e1g1");

        mov = ChessMove::new(Square::E1, Square::F1, PieceType::King);
        assert!(mov.src() == Square::E1);
        assert!(mov.dst() == Square::F1);
        assert!(mov.piece_type() == PieceType::King);
        assert!(mov.promotion().is_none());
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "e1f1");
    }

    #[test]
    fn test_promotion()
    {
        let mut mov = ChessMove::new_promotion(Square::A7, Square::A8, PieceType::Queen);
        assert!(mov.piece_type() == PieceType::Pawn);
        assert!(mov.promotion() == Some(PieceType::Queen));
        assert!(!mov.is_castling());
        assert!(mov.to_string() == "a7a8q");

        mov = ChessMove::new_promotion(Square::D2, Square::D1, PieceType::Knight);
        assert!(mov.piece_type() == PieceType::Pawn);
        assert!(mov.promotion() == Some(PieceType::Knight));
        assert!(mov.to_string() == "d2d1n");
    }

    #[test]
    fn test_castling()
    {
        // King side Castling
        let mut mov = ChessMove::new(Square::E1, Square::G1, PieceType::King);
        assert!(mov.piece_type() == PieceType::King);
        assert!(mov.promotion().is_none());
        assert!(mov.is_castling());
        assert!(mov.to_string() == "e1g1");

        // Queen side Castling
        mov = ChessMove::new(Square::E8, Square::C8, PieceType::King);
        assert!(mov.piece_type() == PieceType::King);
        assert!(mov.promotion().is_none());
        assert!(mov.is_castling());
        assert!(mov.to_string() == "e8c8");
    }

    #[test]
    fn test_move_from_uci()
    {
        // Normal move
        assert!(
            ChessMove::from_uci("e2e4", PieceType::Pawn) ==
            Ok(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn))
        );

        // Knight promotion
        assert!(
            ChessMove::from_uci("a7a8n", PieceType::Pawn) ==
            Ok(ChessMove::new_promotion(Square::A7, Square::A8, PieceType::Knight))
        );

        // Default to queen promotion
        assert!(
            ChessMove::from_uci("a2a1", PieceType::Pawn) ==
            Ok(ChessMove::new_promotion(Square::A2, Square::A1, PieceType::Queen))
        );
    }

}
