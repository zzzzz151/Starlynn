use super::accumulator::Accumulator;
use super::params::HL_SIZE;
use crate::Align64;
use crate::chess::position::Position;

pub type HLActivated = Align64<[[i16; HL_SIZE / 2]; 2]>;

#[repr(C, align(64))]
#[derive(Clone)]
pub struct BothAccumulators {
    white: Accumulator<true>,
    black: Accumulator<false>,
}

impl BothAccumulators {
    pub const fn new() -> Self {
        BothAccumulators {
            white: Accumulator::<true>::new(),
            black: Accumulator::<false>::new(),
        }
    }

    pub fn activated(&self) -> HLActivated {
        Align64([self.white.activated().0, self.black.activated().0])
    }

    pub fn update(&mut self, prev_accs: &BothAccumulators, pos_after_move: &Position) {
        self.white.update(&prev_accs.white, pos_after_move);
        self.black.update(&prev_accs.black, pos_after_move);
    }
}

impl From<&Position> for BothAccumulators {
    fn from(pos: &Position) -> Self {
        BothAccumulators {
            white: Accumulator::<true>::from(pos),
            black: Accumulator::<false>::from(pos),
        }
    }
}
