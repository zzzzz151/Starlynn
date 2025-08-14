use super::params::MAX_DEPTH;
use crate::chess::{chess_move::ChessMove, position::Position, util::FEN_START};
use crate::nn::accumulator::BothAccumulators;
use arrayvec::ArrayVec;
use std::array::from_fn;

pub struct ThreadData {
    pub pos: Position,
    pub(crate) nodes: u64,
    pub(crate) root_depth: i32,
    pub(crate) sel_depth: u32,
    pub(crate) pv_table: [ArrayVec<ChessMove, { MAX_DEPTH as usize + 1 }>; MAX_DEPTH as usize + 1],
    pub(crate) accs_stack: [BothAccumulators; MAX_DEPTH as usize + 1],
}

impl ThreadData {
    pub fn new() -> Self {
        ThreadData {
            pos: Position::try_from(FEN_START).unwrap(),
            nodes: 0,
            root_depth: 1,
            sel_depth: 0,
            pv_table: from_fn(|_| ArrayVec::new()),
            accs_stack: from_fn(|_| BothAccumulators::new()),
        }
    }

    pub fn make_move(&mut self, mov: ChessMove, ply_before: u32, accs_idx_before: usize) {
        self.pos.make_move(mov);
        self.nodes += 1;
        self.sel_depth = self.sel_depth.max(ply_before + 1);

        debug_assert!(accs_idx_before + 1 < self.accs_stack.len());

        unsafe {
            self.accs_stack
                .get_unchecked_mut(accs_idx_before + 1)
                .is_unactivated_updated = false;
        }
    }
}

pub trait AccumulatorsStackExt {
    fn updated_accs(&mut self, pos: &Position, idx: usize) -> &mut BothAccumulators;
}

impl<const N: usize> AccumulatorsStackExt for [BothAccumulators; N] {
    fn updated_accs(
        &mut self,
        pos_after_move: &Position,
        accs_idx: usize,
    ) -> &mut BothAccumulators {
        debug_assert!(accs_idx < self.len());
        let (left, right) = unsafe { self.split_at_mut_unchecked(accs_idx) };

        debug_assert!(!right.is_empty());
        let accs: &mut BothAccumulators = unsafe { right.first_mut().unwrap_unchecked() };

        if !accs.is_unactivated_updated {
            debug_assert!(!left.is_empty());
            let prev_accs: &BothAccumulators = unsafe { left.last().unwrap_unchecked() };
            accs.update(prev_accs, pos_after_move);
        }

        accs
    }
}
