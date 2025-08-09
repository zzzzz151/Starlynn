use crate::chess::{
    position::Position,
    types::{Color, PieceType},
};

use strum::IntoEnumIterator;

const PIECE_VALUES: [i32; 6] = [100, 300, 300, 500, 900, 0];

pub fn evaluate(pos: &Position) -> i32 {
    let stm: Color = pos.side_to_move();
    let mut eval: i32 = 0;

    for pt in PieceType::iter().take(5) {
        let num_ours = pos.piece_bb(stm, pt).count() as i32;
        let num_theirs = pos.piece_bb(!stm, pt).count() as i32;

        eval += (num_ours - num_theirs) * PIECE_VALUES[pt];
    }

    let nudge: i32 = (pos.zobrist_hash() % 21) as i32 - 10;
    eval + nudge
}
