use std::fmt;
use crate::chess_move::ChessMove;
use crate::pos_state::GameState;

#[derive(Debug)]
#[repr(packed)]
pub struct Node {
    pub right_sibling_idx: i32,
    pub first_child_idx: i32,
    pub game_state: GameState,
    pub num_moves: u8,
    pub visits: u32,
    pub total_score: f32,
    pub mov: u16
}

const _: () = assert!(std::mem::size_of::<Node>() == 4 + 4 + 1 + 1 + 4 + 4 + 2);

impl Node {
    pub const fn q(&self) -> f32
    {
        debug_assert!(self.visits > 0);
        self.total_score / (self.visits as f32)
    }

    pub fn puct(&self, child: &Node) -> f32
    {
        debug_assert!(self.visits > 0);
        debug_assert!(self.visits > child.visits);
        debug_assert!(self.num_moves > 0);

        if child.visits == 0 { return f32::MAX; }

        let policy = 1.0 / (self.num_moves as f32);

        let mut u = 1.75 * policy * (self.visits as f32 - 1.0).sqrt();
        u /= child.visits as f32 + 1.0;

        child.q() + u
    }
}

impl fmt::Display for Node
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let right_sibling_idx = self.right_sibling_idx;
        let first_child_idx = self.first_child_idx;
        let game_state = self.game_state;
        let num_moves = self.num_moves;
        let visits = self.visits;
        let mov: ChessMove = self.mov.into();

        let mut str = format!("(right sibling {}, first child {}",
            right_sibling_idx, first_child_idx);

        str += &format!(", game state {}, moves {}",
            game_state, num_moves);

        str += &format!(", visits {}, Q {:.2}, move {})",
            visits, if self.visits == 0 { 0.0 } else { self.q() }, mov);

        write!(f, "{str}")
    }
}
