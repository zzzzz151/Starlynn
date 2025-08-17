use super::params::{MAX_DEPTH, MIN_MATE_SCORE};
use crate::GetCheckedIfDebug;
use crate::chess::{chess_move::ChessMove, position::Position, types::Color, util::FEN_START};
use crate::nn::{accumulator::BothAccumulators, value_policy_heads::value_eval};
use arrayvec::ArrayVec;
use debug_unwraps::DebugUnwrapExt;
use std::array::from_fn;

pub struct StackEntry {
    pub(crate) pv: ArrayVec<ChessMove, { MAX_DEPTH as usize + 1 }>,
    pub(crate) both_accs: BothAccumulators,
    //pub(crate) static_eval: Option<i32>,
    //pub(crate) logits: ArrayVec<(ChessMove, f32), 256>
}

pub struct ThreadData {
    pub pos: Position,
    pub(crate) nodes: u64,
    pub(crate) root_depth: i32,
    pub(crate) sel_depth: u32,
    pub(crate) stack: [StackEntry; MAX_DEPTH as usize + 1],
    pub(crate) lmr_table: [[i32; 256]; MAX_DEPTH as usize + 1],
}

impl ThreadData {
    pub fn new() -> Self {
        let lmr_table = from_fn(|depth| {
            from_fn(|moves_seen| {
                if depth == 0 || moves_seen == 0 {
                    0
                } else {
                    let a: f64 = (depth as f64).ln();
                    let b: f64 = (moves_seen as f64).ln();
                    (0.8 + a * b * 0.4).round() as i32
                }
            })
        });

        ThreadData {
            pos: Position::try_from(FEN_START).unwrap(),
            nodes: 0,
            root_depth: 1,
            sel_depth: 0,
            stack: from_fn(|_| StackEntry {
                pv: ArrayVec::new(),
                both_accs: BothAccumulators::new(),
            }),
            lmr_table,
        }
    }

    pub fn make_move(&mut self, mov: ChessMove, ply_before: u32, accs_idx_before: usize) {
        self.pos.make_move(mov);
        self.nodes += 1;
        self.sel_depth = self.sel_depth.max(ply_before + 1);

        unsafe {
            self.stack
                .get_mut_checked_if_debug(ply_before as usize + 1)
                .pv
                .clear();

            self.stack
                .get_mut_checked_if_debug(accs_idx_before + 1)
                .both_accs
                .set_not_updated();
        }
    }

    pub fn update_both_accs(&mut self, accs_idx: usize) -> &mut BothAccumulators {
        let (left, right) = self.stack.split_at_mut(accs_idx);

        let both_accs: &mut BothAccumulators =
            unsafe { &mut right.first_mut().debug_unwrap_unchecked().both_accs };

        if let Some(prev_stack_entry) = left.last() {
            both_accs.update(&prev_stack_entry.both_accs, &self.pos);
        }

        both_accs
    }

    pub fn static_eval(&mut self, accs_idx: usize) -> i32 {
        let stm: Color = self.pos.side_to_move();
        let both_accs: &mut BothAccumulators = self.update_both_accs(accs_idx);

        value_eval(both_accs, stm).clamp(-MIN_MATE_SCORE + 1, MIN_MATE_SCORE - 1)
    }

    pub fn update_pv(&mut self, ply: usize, mov: ChessMove) {
        unsafe {
            let child_pv = self.stack.get_checked_if_debug(ply + 1).pv.clone();
            let pv = &mut self.stack.get_mut_checked_if_debug(ply).pv;

            pv.clear();
            pv.push_unchecked(mov);

            for next_move in child_pv {
                pv.push_unchecked(next_move);
            }
        }
    }
}
