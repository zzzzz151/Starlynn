use super::params::MIN_MATE_SCORE;
use crate::chess::chess_move::ChessMove;

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[rustfmt::skip]
pub enum Bound {
    Exact, Lower, Upper,
}

const _: () = assert!(size_of::<Option<Bound>>() == 1);

// Transposition table entry
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct TTEntry {
    zobrist_hash: u64,
    depth: u8,
    score: i16,
    bound: Option<Bound>,
    mov: Option<ChessMove>,
}

const _: () = assert!(size_of::<TTEntry>() == 8 + 1 + 2 + 1 + 2);

impl TTEntry {
    pub const fn new() -> Self {
        TTEntry {
            zobrist_hash: 0,
            depth: 0,
            score: 0,
            bound: None,
            mov: None,
        }
    }

    // Returns depth, score, bound, move
    pub const fn get(
        &self,
        zobrist_hash: u64,
        ply: u32,
    ) -> (i32, i32, Option<Bound>, Option<ChessMove>) {
        if let Some(bound) = self.bound
            && zobrist_hash == self.zobrist_hash
        {
            let mut score: i32 = self.score as i32;

            // Fix mate scores
            if score >= MIN_MATE_SCORE {
                score -= ply as i32;
            } else if score <= -MIN_MATE_SCORE {
                score += ply as i32;
            }

            (self.depth as i32, score, Some(bound), self.mov)
        } else {
            (0, 0, None, None)
        }
    }

    pub const fn has_entry(&self) -> bool {
        self.bound.is_some()
    }

    pub fn update(
        &mut self,
        new_hash: u64,
        new_depth: u8,
        mut new_score: i16,
        ply: u32,
        new_bound: Bound,
        new_move: Option<ChessMove>,
    ) {
        // Fix mate scores
        if new_score as i32 >= MIN_MATE_SCORE {
            new_score += ply as i16;
        } else if new_score as i32 <= -MIN_MATE_SCORE {
            new_score -= ply as i16;
        }

        *self = TTEntry {
            zobrist_hash: new_hash,
            depth: new_depth,
            score: new_score,
            bound: Some(new_bound),
            mov: new_move.or(self.mov),
        };
    }
}
