use std::fmt;
use crate::chess_move::ChessMove;
use crate::types::GameState;

#[derive(Debug)]
#[repr(packed)]
pub struct Node {
    pub first_child_idx: i32,
    pub num_moves: u8,
    pub game_state: GameState,
    pub mov: u16,
    pub visits: u32,
    pub total_score: f32
}

const _: () = assert!(std::mem::size_of::<Node>() == 4 + 1 + 1 + 4 + 4 + 2);

impl Node {
    pub fn iter(&self) -> NodeIterator
    {
        debug_assert!(self.num_moves != 0  || self.game_state.is_terminal());

        if self.first_child_idx == -1
        {

            NodeIterator {
                next_child_idx: 1,
                last_child_idx: 0
            }
        }
        else {
            debug_assert!(self.num_moves > 0 && self.num_moves != u8::MAX);
            debug_assert!(self.game_state == GameState::Ongoing);
            debug_assert!(self.first_child_idx == 1 || self.visits >= 1);

            let first_child_idx = self.first_child_idx as usize;

            NodeIterator {
                next_child_idx: first_child_idx,
                last_child_idx: first_child_idx + (self.num_moves as usize) - 1,
            }
        }
    }

    pub const fn q(&self) -> f32
    {
        debug_assert!(self.visits > 0 && self.total_score >= 0.0);
        debug_assert!(self.total_score <= (self.visits as f32));

        self.total_score / (self.visits as f32)
    }

    pub fn puct(&self, child: &Node) -> f32
    {
        debug_assert!(self.num_moves > 0);
        debug_assert!(self.visits > child.visits);

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
        let first_child_idx = self.first_child_idx;
        let num_moves = self.num_moves;
        let game_state = self.game_state;
        let mov: ChessMove = self.mov.into();
        let visits = self.visits;

        let mut str = format!("(first child {}. moves {}, game state {}",
            first_child_idx, num_moves, game_state);

        str += &format!(", move {}, visits {}, Q {:.2})",
            mov, visits, if visits == 0 { f32::NAN } else { self.q() });

        write!(f, "{str}")
    }
}

pub struct NodeIterator {
    next_child_idx: usize,
    last_child_idx: usize
}

impl Iterator for NodeIterator {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item>
    {
        if self.next_child_idx > self.last_child_idx {
            None
        }
        else {
            self.next_child_idx += 1;
            Some(self.next_child_idx - 1)
        }
    }
}
