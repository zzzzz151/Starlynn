use super::bitboard::Bitboard;
use super::chess_move::ChessMove;
use super::pos_state::PosState;
use super::types::{Color, PieceType, Square};
use arrayvec::ArrayVec;
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
            debug_assert!(!self.0.is_empty());
            unsafe { self.0.last().unwrap_unchecked() }
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
            pub fn is_capture(&self, mov: ChessMove) -> bool;
            pub fn fen(&self) -> String;
            pub fn display(&self);
            pub fn attacks(&self, color: Color, occ: Bitboard) -> Bitboard;
            pub fn attackers(&self, sq: Square) -> Bitboard;
            pub fn pinned(&self) -> (Bitboard, Bitboard);
        }
    }

    pub fn state(&self, n_states_ago: usize) -> Option<&PosState> {
        if n_states_ago >= self.0.len() {
            return None;
        }

        let state: &PosState = unsafe { self.0.get_unchecked(self.0.len() - n_states_ago - 1) };
        Some(state)
    }

    pub fn legal_moves(&self) -> ArrayVec<ChessMove, 256> {
        debug_assert!(!self.0.is_empty());
        unsafe { self.0.last().unwrap_unchecked().legal_moves() }
    }

    pub fn make_move(&mut self, mov: ChessMove) {
        debug_assert!(!self.0.is_empty());
        let mut new_state: PosState = unsafe { self.0.last().unwrap_unchecked().clone() };
        new_state.make_move(mov);
        self.0.push(new_state);
    }

    pub fn undo_move(&mut self) {
        if self.0.len() > 1 {
            self.0.truncate(self.0.len() - 1)
        }
    }

    pub fn is_insufficient_material(&self) -> bool {
        let num_pieces = self.occupancy().count();

        // KvK
        if num_pieces == 2 {
            return true;
        }

        let num_knights = self.piece_type_bb(PieceType::Knight).count();
        let num_bishops = self.piece_type_bb(PieceType::Bishop).count();

        // KvN or KvB
        if num_pieces == 3 && (num_knights == 1 || num_bishops == 1) {
            return true;
        }

        // KvNN or NvN or BvB or NvB
        if num_pieces == 4 {
            if num_knights == 2 {
                return true;
            }

            if self.us().count() == 2
                && (num_bishops == 2 || (num_knights == 1 && num_bishops == 1))
            {
                return true;
            }
        }

        false
    }

    pub fn is_repetition(&self) -> bool {
        debug_assert!(!self.0.is_empty());
        let current_hash: u64 = unsafe { self.0.last().unwrap_unchecked().zobrist_hash() };

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
