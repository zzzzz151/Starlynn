use arrayvec::ArrayVec;
use crate::position::Position;
use crate::chess_move::{ChessMove};

impl Position {
    pub fn legal_moves(&self) -> ArrayVec<ChessMove, 256>
    {
        let legal_moves = ArrayVec::<ChessMove, 256>::new();

        // King moves



        legal_moves
    }
}