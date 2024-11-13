use arrayvec::ArrayVec;
use crate::types::{Color, Square, PieceType};
use crate::bitboard::Bitboard;
use crate::pos_state::*;
use crate::chess_move::ChessMove;

#[derive(Clone, Debug)]
pub struct Position {
    state: PosState,
    hashes_exclusive: Vec<u64>
}

impl TryFrom<&str> for Position {
    type Error = InvalidFEN;

    fn try_from(fen: &str) -> Result<Self, Self::Error>
    {
        let pos = Self {
            state: PosState::try_from(fen)?,
            hashes_exclusive: Vec::with_capacity(512)
        };

        Ok(pos)
    }
}

#[allow(dead_code)]
impl Position {
    pub const fn stm(&self) -> Color { self.state.stm() }

    pub fn color_bb(&self, color: Color) -> Bitboard {
        self.state.color_bb(color)
    }

    pub fn piece_type_bb(&self, piece_type: PieceType) -> Bitboard {
         self.state.piece_type_bb(piece_type)
    }

    pub fn piece_bb(&self, color: Color, piece_type: PieceType) -> Bitboard {
        self.state.piece_bb(color, piece_type)
    }

    pub fn us(&self) -> Bitboard { self.state.us() }

    pub fn them(&self) -> Bitboard { self.state.them() }

    pub fn occupancy(&self) -> Bitboard { self.state.occupancy() }

    pub fn has_castling_right(&self, color: Color, queen_side_castle: bool) -> bool {
        self.state.has_castling_right(color, queen_side_castle)
    }

    pub const fn en_passant_square(&self) -> Option<Square> { self.state.en_passant_square() }

    pub const fn plies_since_pawn_or_capture(&self) -> u16 {
        self.state.plies_since_pawn_or_capture()
    }

    pub const fn checkers(&self) -> Bitboard { self.state.checkers() }

    pub fn in_check(&self) -> bool { self.state.in_check() }

    pub const fn zobrist_hash(&self) -> u64 { self.state.zobrist_hash() }

    pub fn at(&self, square: Square) -> Option<PieceType> { self.state.at(square) }

    pub fn king_square(&self, color: Color) -> Square { self.state.king_square(color) }

    pub fn toggle_piece(&mut self, color: Color, pt: PieceType, square: Square) {
        self.state.toggle_piece(color, pt, square)
    }

    pub fn fen(&self) -> String { self.state.fen() }

    pub fn display(&self) { self.state.display(); }

    pub fn attacks(&self, color: Color, occupancy: Bitboard) -> Bitboard {
        self.state.attacks(color, occupancy)
    }

    pub fn attackers(&self, square: Square) -> Bitboard { self.state.attackers(square) }

    pub fn is_draw(&self, num_moves: usize) -> bool {
        self.state.is_draw(num_moves, &self.hashes_exclusive)
    }

    pub fn make_move(&mut self, mov: ChessMove)
    {
        self.hashes_exclusive.push(self.state.zobrist_hash());
        self.state.make_move(mov);

        if self.state.plies_since_pawn_or_capture() == 0 {
            self.hashes_exclusive.clear();
        }
    }

    pub fn uci_to_move(&self, uci_move: &str) -> Result<ChessMove, InvalidUciMove> {
        self.state.uci_to_move(uci_move)
    }

    pub fn moves(&mut self, underpromos: bool) -> ArrayVec<ChessMove, 256> {
        self.state.moves(underpromos)
    }

    pub fn pinned(&self) -> (Bitboard, Bitboard) { self.state.pinned() }

    pub fn perft(&mut self, depth: u8) -> u64 { self.state.perft(depth) }

    pub fn perft_split(&mut self, depth: u8) -> u64 { self.state.perft_split(depth) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_draw() {
        // KvK
        assert!(Position::try_from("4k3/8/8/8/8/8/8/4K3 w - - 0 1")
            .unwrap().is_draw(5)
        );

        // KvR
        assert!(!Position::try_from("4k3/8/8/8/8/8/8/3RK3 w - - 0 1")
            .unwrap().is_draw(14)
        );

        // KvR, 50 moves rule
        assert!(Position::try_from("4k3/8/8/8/8/8/8/3RK3 w - - 100 1")
            .unwrap().is_draw(14)
        );

        // Checkmate, 50 moves rule
        assert!(!Position::try_from("8/8/8/8/8/8/5KQ1/7k b - - 100 1")
            .unwrap().is_draw(0)
        );

        // Repetition

        let mut pos = Position::try_from(START_FEN).unwrap();
        assert!(!pos.is_draw(20));

        pos.make_move(ChessMove::new(Square::B1, Square::C3, PieceType::Knight));
        assert!(!pos.is_draw(20));

        pos.make_move(ChessMove::new(Square::B8, Square::C6, PieceType::Knight));
        assert!(!pos.is_draw(22));

        pos.make_move(ChessMove::new(Square::C3, Square::B1, PieceType::Knight));
        assert!(!pos.is_draw(22));

        let pos_before_repetition = pos.clone();

        pos.make_move(ChessMove::new(Square::C6, Square::B8, PieceType::Knight));
        assert!(pos.is_draw(20));

        let pos_at_repetition = pos.clone();

        pos.make_move(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn));
        assert!(!pos.is_draw(20));

        pos = pos_at_repetition.clone();

        pos.make_move(ChessMove::new(Square::B1, Square::C3, PieceType::Knight));
        assert!(pos.is_draw(20));

        pos = pos_before_repetition.clone();

        pos.make_move(ChessMove::new(Square::E7, Square::E5, PieceType::Pawn));
        assert!(!pos.is_draw(20));
    }
}
