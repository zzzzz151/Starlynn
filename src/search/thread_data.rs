use super::params::{CORR_HIST_SIZE, MAX_DEPTH, MIN_MATE_SCORE};
use crate::GetCheckedIfDebug;
use crate::chess::{chess_move::ChessMove, position::Position, types::Color, util::FEN_START};
use crate::nn::{accumulator::BothAccumulators, value_policy_heads::value_eval};
use arrayvec::ArrayVec;
use debug_unwraps::DebugUnwrapExt;
use std::array::from_fn;

pub struct StackEntry {
    pub(crate) pv: ArrayVec<ChessMove, { MAX_DEPTH as usize + 1 }>, // Principal variation
    pub(crate) both_accs: BothAccumulators,
    pub(crate) raw_eval: Option<i32>,
    // pub(crate) scored_moves: Option<ScoredMoves>
}

pub struct ThreadData {
    pub pos: Position,
    pub(crate) nodes: u64,
    pub(crate) root_depth: i32,
    pub(crate) sel_depth: u32,
    pub(crate) stack: [StackEntry; MAX_DEPTH as usize + 1], // [ply] or [accs_idx]
    pub(crate) lmr_table: [[i32; 256]; MAX_DEPTH as usize + 1], // [depth][moves_seen]
    pawns_kings_corr_hist: [[i16; CORR_HIST_SIZE]; 2],      // [stm]
    non_pawns_corr_hist: [[[i16; CORR_HIST_SIZE]; 2]; 2],   // [stm][piece_color][color_pieces_hash]
}

impl ThreadData {
    pub fn new() -> Self {
        // Compute base LMR
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
                pv: ArrayVec::new_const(),
                both_accs: BothAccumulators::new(),
                raw_eval: None,
            }),
            lmr_table,
            pawns_kings_corr_hist: [[0; CORR_HIST_SIZE]; 2],
            non_pawns_corr_hist: [[[0; CORR_HIST_SIZE]; 2]; 2],
        }
    }

    pub fn ucinewgame(&mut self) {
        self.pos = Position::try_from(FEN_START).unwrap();
        self.pawns_kings_corr_hist = [[0; CORR_HIST_SIZE]; 2];
        self.non_pawns_corr_hist = [[[0; CORR_HIST_SIZE]; 2]; 2];
    }

    pub fn make_move(&mut self, mov: Option<ChessMove>, ply_before: u32, accs_idx_before: usize) {
        if let Some(mov) = mov {
            self.pos.make_move(mov);
        } else {
            self.pos.make_null_move();
        }

        self.nodes += 1;
        self.sel_depth = self.sel_depth.max(ply_before + 1);

        unsafe {
            let new_stack_entry: &mut StackEntry =
                self.stack.get_mut_checked_if_debug(ply_before as usize + 1);

            new_stack_entry.pv.clear();
            new_stack_entry.raw_eval = None;

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

    pub fn pawns_kings_corr(&mut self) -> &mut i16 {
        let idx: usize = self.pos.pawns_kings_hash() as usize % CORR_HIST_SIZE;
        unsafe { self.pawns_kings_corr_hist[self.pos.side_to_move()].get_unchecked_mut(idx) }
    }

    pub fn non_pawns_corr(&mut self, piece_color: Color) -> &mut i16 {
        let idx: usize = self.pos.non_pawns_hash(piece_color) as usize % CORR_HIST_SIZE;

        unsafe {
            self.non_pawns_corr_hist[self.pos.side_to_move()][piece_color].get_unchecked_mut(idx)
        }
    }

    fn eval_correction(&mut self) -> i32 {
        let pawns_kings_corr: i32 = *(self.pawns_kings_corr()) as i32;
        let w_non_pawns_corr: i32 = *(self.non_pawns_corr(Color::White)) as i32;
        let b_non_pawns_corr: i32 = *(self.non_pawns_corr(Color::Black)) as i32;

        (pawns_kings_corr + w_non_pawns_corr + b_non_pawns_corr) / 100
    }

    // Returns static eval if cached, else computes it, caches it, and returns it
    pub fn static_eval(&mut self, ply: usize, accs_idx: usize) -> i32 {
        let stack_entry: &StackEntry = unsafe { self.stack.get_checked_if_debug(ply) };

        if let Some(raw_eval) = stack_entry.raw_eval {
            return raw_eval + self.eval_correction();
        }

        let raw_eval: i32 = {
            let stm: Color = self.pos.side_to_move();
            let both_accs: &mut BothAccumulators = self.update_both_accs(accs_idx);
            value_eval(both_accs, stm).clamp(-MIN_MATE_SCORE + 1, MIN_MATE_SCORE - 1)
        };

        let stack_entry: &mut StackEntry = unsafe { self.stack.get_mut_checked_if_debug(ply) };
        stack_entry.raw_eval = Some(raw_eval);

        raw_eval + self.eval_correction()
    }

    // Update principal variation
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
