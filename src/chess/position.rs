use super::bitboard::Bitboard;
use super::chess_move::ChessMove;
use super::pos_state::PosState;
use super::types::{Color, PieceType, Square};
use arrayvec::ArrayVec;
use debug_unwraps::DebugUnwrapExt;
use delegate::delegate;

#[derive(Clone)]
pub struct Position(Vec<PosState>);

impl TryFrom<&str> for Position {
    type Error = String;

    fn try_from(fen: &str) -> Result<Self, Self::Error> {
        let mut pos = Position(Vec::with_capacity(512));
        pos.0.push(PosState::try_from(fen)?);
        Ok(pos)
    }
}

#[allow(dead_code)]
impl Position {
    delegate! {
        to {
            unsafe { self.0.last().debug_unwrap_unchecked() }
        } {
            pub fn side_to_move(&self) -> Color;
            pub fn color_bb(&self, color: Color) -> Bitboard;
            pub fn piece_type_bb(&self, pt: PieceType) -> Bitboard;
            pub fn piece_bb(&self, color: Color, pt: PieceType) -> Bitboard;
            pub fn us(&self) -> Bitboard;
            pub fn them(&self) -> Bitboard;
            pub fn occupancy(&self) -> Bitboard;
            pub fn en_passant_square(&self) -> Option<Square>;
            pub fn plies_since_pawn_or_capture(&self) -> u16;
            pub fn last_move(&self) -> Option<ChessMove>;
            pub fn piece_type_captured(&self) -> Option<PieceType>;
            pub fn piece_type_captured_by(&self, mov: ChessMove) -> Option<PieceType>;
            pub fn in_check(&self) -> bool;
            pub fn zobrist_hash(&self) -> u64;
            pub fn at(&self, sq: Square) -> Option<PieceType>;
            pub fn color_at(&self, sq: Square) -> Option<Color>;
            pub fn king_square(&self, color: Color) -> Square;
            pub fn has_nbrq(&self, color: Color) -> bool;
            pub fn is_capture(&self, mov: ChessMove) -> bool;
            pub fn is_noisy_not_underpromotion(&self, mov: ChessMove) -> bool;
            pub fn fen(&self) -> String;
            pub fn display(&self);
            pub fn is_insufficient_material(&self) -> bool;
            pub fn attacks(&self, color: Color, occ: Bitboard) -> Bitboard;
            pub fn attackers(&self, sq: Square, occupancy: Bitboard) -> Bitboard;
            pub fn pinned(&self) -> (Bitboard, Bitboard);
            pub fn see_ge(&self, mov: ChessMove, threshold: i32) -> bool;
        }
    }

    pub fn state<const N_STATES_AGO: usize>(&self) -> Option<&PosState> {
        if N_STATES_AGO >= self.0.len() {
            return None;
        }

        let state: &PosState = unsafe { self.0.get_unchecked(self.0.len() - N_STATES_AGO - 1) };
        Some(state)
    }

    pub fn legal_moves(&self) -> ArrayVec<ChessMove, 256> {
        unsafe { self.0.last().debug_unwrap_unchecked().legal_moves() }
    }

    pub fn make_move(&mut self, mov: ChessMove) {
        let mut new_state: PosState = unsafe { self.0.last().debug_unwrap_unchecked().clone() };
        new_state.make_move(mov);
        self.0.push(new_state);
    }

    pub fn make_null_move(&mut self) {
        let mut new_state: PosState = unsafe { self.0.last().debug_unwrap_unchecked().clone() };
        new_state.make_null_move();
        self.0.push(new_state);
    }

    pub fn undo_move(&mut self) {
        if self.0.len() > 1 {
            self.0.truncate(self.0.len() - 1)
        }
    }

    pub fn is_repetition(&self) -> bool {
        let current_hash: u64 = unsafe { self.0.last().debug_unwrap_unchecked().zobrist_hash() };

        self.0
            .iter()
            .rev()
            .skip(2)
            .step_by(2)
            .any(|pos_state| pos_state.zobrist_hash() == current_hash)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess::util::{FEN_KIWIPETE, FEN_START};

    #[test]
    fn test_insufficient_material() {
        // Is sufficient material
        for fen in [
            FEN_START,
            FEN_KIWIPETE,
            "8/3k4/8/8/8/3KBN2/8/8 w - - 0 1",  // KvNB
            "8/3k4/8/8/8/3KBN2/8/8 b - - 0 1",  // KvNB
            "8/3k4/8/8/8/3KBB2/8/8 w - - 0 1",  // KvBB
            "8/3k4/8/8/8/3KBB2/8/8 b - - 0 1",  // KvBB
            "8/8/4k3/8/8/8/3RK3/8 w - - 0 1",   // KvR
            "8/8/4k3/8/8/8/3RK3/8 b - - 0 1",   // KvR
            "8/8/3kn3/8/8/2BBK3/8/8 w - - 0 1", // NvBB
            "8/8/3kn3/8/8/2BBK3/8/8 b - - 0 1", // NvBB
            "8/8/3kn3/8/8/2NBK3/8/8 w - - 0 1", // NvNB
            "8/8/3kn3/8/8/2NBK3/8/8 b - - 0 1", // NvNB
            "5Q1k/8/4n1K1/8/8/8/8/8 b - - 0 1",
        ] {
            let pos = Position::try_from(fen).unwrap();
            assert!(!pos.is_insufficient_material());
        }

        // Is insufficient material
        for fen in [
            "8/8/5k2/8/8/3K4/8/8 w - - 0 1",   // KvK
            "8/2N1K3/8/8/8/3k4/8/8 w - - 0 1", // KvN
            "8/2N1K3/8/8/8/3k4/8/8 b - - 0 1", // KvN
            "8/4K3/8/8/8/2bk4/8/8 w - - 0 1",  // KvB
            "8/4K3/8/8/8/2bk4/8/8 b - - 0 1",  // KvB
            "8/2NNK3/8/8/8/3k4/8/8 w - - 0 1", // KvNN
            "8/2NNK3/8/8/8/3k4/8/8 b - - 0 1", // KvNN
            "8/3NK3/8/8/8/2nk4/8/8 w - - 0 1", // NvN
            "8/3NK3/8/8/8/2nk4/8/8 b - - 0 1", // NvN
            "8/3BK3/8/8/8/2bk4/8/8 w - - 0 1", // BvB
            "8/3BK3/8/8/8/2bk4/8/8 b - - 0 1", // BvB
            "8/3BK3/8/8/8/2nk4/8/8 w - - 0 1", // NvB
            "8/3BK3/8/8/8/2nk4/8/8 b - - 0 1", // NvB
        ] {
            let pos = Position::try_from(fen).unwrap();
            assert!(pos.is_insufficient_material());
        }
    }

    #[test]
    fn test_repetition() {
        let mut pos = Position::try_from(FEN_START).unwrap();
        assert!(!pos.is_repetition());

        pos.make_move(ChessMove::new(Square::G1, Square::F3, PieceType::Knight));
        assert!(!pos.is_repetition());

        pos.make_move(ChessMove::new(Square::G8, Square::F6, PieceType::Knight));
        assert!(!pos.is_repetition());

        pos.make_move(ChessMove::new(Square::F3, Square::G1, PieceType::Knight));
        assert!(!pos.is_repetition());

        let pos_before_repetition = pos.clone();

        pos.make_move(ChessMove::new(Square::F6, Square::G8, PieceType::Knight));
        assert!(pos.is_repetition());

        pos.make_move(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn));
        assert!(!pos.is_repetition());

        pos.make_move(ChessMove::new(Square::G8, Square::F6, PieceType::Knight));
        assert!(!pos.is_repetition());

        pos.make_move(ChessMove::new(Square::G1, Square::F3, PieceType::Knight));
        assert!(!pos.is_repetition());

        pos = pos_before_repetition;

        pos.make_move(ChessMove::new(Square::B8, Square::C6, PieceType::Knight));
        assert!(!pos.is_repetition());

        pos.make_move(ChessMove::new(Square::G1, Square::F3, PieceType::Knight));
        assert!(!pos.is_repetition());
    }
}
