use std::fmt;
use crate::chess_move::ChessMove;
use crate::pos_state::GameState;

#[derive(Debug)]
#[repr(packed)]
pub struct Node {
    pub right_sibling_idx: i32,
    pub first_child_idx: i32,
    pub game_state: GameState,
    pub visits: u32,
    pub total_score: f32,
    pub mov: u16
}

const _: () = assert!(std::mem::size_of::<Node>() == 4 + 4 + 1 + 4 + 4 + 2);

impl Node {
    pub fn score<T: num_traits::Float>(&self) -> T
    {
        if self.visits == 0 { return T::from(1.0).unwrap(); }

        debug_assert!(T::from(self.total_score).unwrap() <= T::from(self.visits).unwrap());

        T::from(self.total_score).unwrap() / T::from(self.visits).unwrap()
    }

    pub fn uct(&self, parent_visits: u32) -> f32
    {
        debug_assert!(parent_visits > 0);
        debug_assert!(self.visits <= parent_visits);

        if self.visits == 0 { return f32::MAX; }

        let visits_ratio = (parent_visits as f32).ln() / (self.visits as f32);

        self.score::<f32>() + 1.75 * visits_ratio.sqrt()
    }
}

impl fmt::Display for Node
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let right_sibling_idx = self.right_sibling_idx;
        let first_child_idx = self.first_child_idx;
        let game_state = self.game_state;
        let visits = self.visits;
        let total_score = self.total_score;
        let mov: ChessMove = self.mov.into();

        let mut str = format!("(right sibling {}, first child {}, game state {}",
            right_sibling_idx, first_child_idx, game_state);

        str += &format!(", visits {}, total score {:.2}, score {:.2}, move {})",
            visits, total_score, self.score::<f64>(), mov);

        write!(f, "{str}")
    }
}
