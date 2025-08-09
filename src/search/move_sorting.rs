use crate::chess::{chess_move::ChessMove, position::Position, types::PieceType};
use arrayvec::ArrayVec;

pub fn get_scored_moves(
    pos: &Position,
    moves: &ArrayVec<ChessMove, 256>,
) -> ArrayVec<(ChessMove, i32), 256> {
    let mut scored_moves: ArrayVec<(ChessMove, i32), 256> = ArrayVec::new();

    for &mov in moves {
        let score: i32 = if let Some(promo_pt) = mov.promotion() {
            10_000 * if promo_pt == PieceType::Queen { 1 } else { -1 }
        } else if let Some(pt_captured) = pos.captured(mov) {
            // MVVLVA

            let pt_moving_value: i32 = match mov.piece_type() {
                PieceType::King => 0,
                pt => pt as i32 + 1,
            };

            1000 + (pt_captured as i32) * 100 - pt_moving_value
        } else {
            0
        };

        scored_moves.push((mov, score));
    }

    scored_moves
}

pub fn remove_best_move(scored_moves: &mut ArrayVec<(ChessMove, i32), 256>) -> Option<ChessMove> {
    if scored_moves.is_empty() {
        return None;
    }

    let mut best_idx: usize = 0;

    for i in 1..scored_moves.len() {
        if scored_moves[i].1 > scored_moves[best_idx].1 {
            best_idx = i;
        }
    }

    Some(scored_moves.swap_remove(best_idx).0)
}
