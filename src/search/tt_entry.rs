use super::params::MIN_MATE_SCORE;

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
}

const _: () = assert!(size_of::<TTEntry>() == 8 + 1 + 2 + 1);

impl TTEntry {
    pub fn new() -> Self {
        TTEntry {
            zobrist_hash: 0,
            depth: 0,
            score: 0,
            bound: None,
        }
    }

    pub const fn get(&self, zobrist_hash: u64, ply: u32) -> Option<(i32, i32, Bound)> {
        if let Some(bound) = self.bound
            && zobrist_hash == self.zobrist_hash
        {
            let mut score: i32 = self.score as i32;

            if score >= MIN_MATE_SCORE {
                score -= ply as i32;
            } else if score <= -MIN_MATE_SCORE {
                score += ply as i32;
            }

            Some((self.depth as i32, score, bound))
        } else {
            None
        }
    }

    pub const fn has_entry(&self) -> bool {
        self.bound.is_some()
    }

    pub const fn update(
        &mut self,
        new_hash: u64,
        new_depth: u8,
        mut new_score: i16,
        ply: u32,
        new_bound: Bound,
    ) {
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
        };
    }
}
