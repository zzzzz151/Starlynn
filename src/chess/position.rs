use delegate::delegate;
use arrayvec::ArrayVec;
use super::types::*;
use super::bitboard::Bitboard;
use super::chess_move::ChessMove;
use super::pos_state::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GameState {
    Ongoing, Draw, Lost
}

#[derive(Clone, Debug)]
pub struct Position
{
    state: PosState,
    hashes_after_pawn_move_or_capture: Vec<u64>
}

impl TryFrom<&str> for Position
{
    type Error = InvalidFEN;

    fn try_from(fen: &str) -> Result<Self, Self::Error>
    {
        Ok(Position {
            state: PosState::try_from(fen)?,
            hashes_after_pawn_move_or_capture: Vec::with_capacity(512)
         })
    }
}

impl Position
{
    delegate! {
        to self.state {
            pub const fn stm(&self) -> Color;
            pub fn color_bb(&self, color: Color) -> Bitboard;
            pub fn piece_type_bb(&self, pt: PieceType) -> Bitboard;
            pub fn piece_bb(&self, color: Color, pt: PieceType) -> Bitboard;
            pub fn us(&self) -> Bitboard;
            pub fn them(&self) -> Bitboard;
            pub fn occupancy(&self) -> Bitboard;
            pub const fn en_passant_square(&self) -> Option<Square>;
            pub const fn plies_since_pawn_or_capture(&self) -> u16;
            pub const fn last_move(&self) -> Option<ChessMove>;
            pub const fn in_check(&self) -> bool;
            pub const fn zobrist_hash(&self) -> u64;
            pub fn at(&self, sq: Square) -> Option<PieceType>;
            pub fn king_square(&self, color: Color) -> Square;
            pub fn fen(&self) -> String;
            pub fn display(&self);
            pub fn attacks(&self, color: Color, occ: Bitboard) -> Bitboard;
            pub fn attackers(&self, sq: Square) -> Bitboard;
            pub fn pinned(&self) -> (Bitboard, Bitboard);
            pub fn legal_moves(&self, underpromos: bool) -> ArrayVec<ChessMove, 256>;
            pub fn has_legal_move(&self) -> bool;
        }
    }

    pub fn state(&self) -> &PosState { &(self.state) }

    pub fn make_move(&mut self, mov: ChessMove)
    {
        self.hashes_after_pawn_move_or_capture.push(self.zobrist_hash());
        self.state.make_move(mov);

        if self.plies_since_pawn_or_capture() == 0
        {
            self.hashes_after_pawn_move_or_capture.clear();
        }
    }

    pub fn game_state(&self) -> GameState
    {
        debug_assert!(self.piece_type_bb(PieceType::King).count() == 2);

        // Checkmate or stalemate
        if !self.has_legal_move()
        {
            return if self.in_check() { GameState::Lost } else { GameState::Draw };
        }

        // 50 moves rule
        if self.plies_since_pawn_or_capture() >= 100
        {
            return GameState::Draw;
        }

        let num_pieces = self.occupancy().count();

        // KvK
        if num_pieces == 2 { return GameState::Draw; }

        let num_knights = self.piece_type_bb(PieceType::Knight).count();
        let num_bishops = self.piece_type_bb(PieceType::Bishop).count();

        // KvN or KvB
        if num_pieces == 3 && (num_knights == 1 || num_bishops == 1)
        {
            return GameState::Draw;
        }

        // KvNN or NvN or BvB or NvB
        if num_pieces == 4
        {
            if num_knights == 2 { return GameState::Draw; }

            if self.us().count() == 2
            && (num_bishops == 2 || (num_knights == 1 && num_bishops == 1))
            {
                return GameState::Draw;
            }
        }

        // Draw by repetition
        for zobrist_hash in self.hashes_after_pawn_move_or_capture.iter().rev().skip(1).step_by(2)
        {
            if self.zobrist_hash() == *zobrist_hash
            {
                return GameState::Draw;
            }
        }

        GameState::Ongoing
    }

}

#[cfg(test)]
mod tests
{
    use super::*;
    use crate::chess::util::{FEN_KIWIPETE, FEN_START};

    #[test]
    fn test_game_state()
    {
        // Ongoing
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
            "5Q1k/8/4n1K1/8/8/8/8/8 b - - 0 1"
        ] {
            let pos = Position::try_from(fen).unwrap();
            assert!(pos.game_state() == GameState::Ongoing);
        }

        // Draws
        for fen in [
            "8/8/8/8/8/6Q1/8/5K1k b - - 0 1", // Stalemate
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 100 1", // 50 moves rule
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
            assert!(pos.game_state() == GameState::Draw);
        }

        // Checkmates
        for fen in [
            "8/8/8/8/8/KN1B4/8/k7 b - - 0 1",
            "8/8/8/8/8/KN1B4/8/k7 b - - 100 1",
            "8/8/8/4n3/8/K7/8/k1Q5 b - - 0 1",
            "8/8/8/4n3/8/K7/8/k1Q5 b - - 100 1",
        ] {
            let pos = Position::try_from(fen).unwrap();
            assert!(pos.game_state() == GameState::Lost);
        }

        // Draw by repetition

        let mut pos = Position::try_from(FEN_START).unwrap();
        assert!(pos.game_state() == GameState::Ongoing);

        pos.make_move(ChessMove::new(Square::G1, Square::F3, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);

        pos.make_move(ChessMove::new(Square::G8, Square::F6, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);

        pos.make_move(ChessMove::new(Square::F3, Square::G1, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);

        let pos_before_repetition = pos.clone();

        pos.make_move(ChessMove::new(Square::F6, Square::G8, PieceType::Knight));
        assert!(pos.game_state() == GameState::Draw);

        pos.make_move(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn));
        assert!(pos.game_state() == GameState::Ongoing);

        pos.make_move(ChessMove::new(Square::G8, Square::F6, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);

        pos.make_move(ChessMove::new(Square::G1, Square::F3, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);

        pos = pos_before_repetition;

        pos.make_move(ChessMove::new(Square::B8, Square::C6, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);

        pos.make_move(ChessMove::new(Square::G1, Square::F3, PieceType::Knight));
        assert!(pos.game_state() == GameState::Ongoing);
    }

}
