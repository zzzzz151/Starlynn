use super::params::*;
use crate::GetCheckedIfDebug;
use crate::chess::{chess_move::ChessMove, position::Position, types::Color, util::FEN_START};
use arrayvec::ArrayVec;
use debug_unwraps::DebugUnwrapExt;
use std::array::from_fn;

use crate::nn::{
    both_accumulators::{BothAccumulators, HLActivated},
    value_policy_heads::value_eval,
};

pub struct StackEntry {
    pub(crate) pv: ArrayVec<ChessMove, { MAX_DEPTH as usize + 1 }>, // Principal variation
    pub(crate) both_accs: BothAccumulators,
    pub(crate) hl_activated: HLActivated,
    pub(crate) is_hl_updated: bool,
    pub(crate) raw_eval: Option<i32>,
    // pub(crate) scored_moves: Option<ScoredMoves>
}

pub struct ThreadData {
    pub pos: Position,
    pub(crate) nodes: u64,
    pub(crate) nodes_by_move: [u64; u16::MAX as usize],
    pub(crate) root_depth: i32,
    pub(crate) sel_depth: u32,
    pub(crate) stack: [StackEntry; MAX_DEPTH as usize + 1], // [ply] or [accs_idx]
    pub(crate) lmr_table: [[[i32; 2]; 256]; MAX_DEPTH as usize + 1], // [depth][moves_seen][is_noisy]
    pawns_kings_corr_hist: [[i16; CORR_HIST_SIZE]; 2],               // [stm]
    non_pawns_corr_hist: [[[i16; CORR_HIST_SIZE]; 2]; 2], // [stm][piece_color][color_pieces_hash]
    last_move_corr_hist: [[[i16; 64]; 6]; 2], // [stm][last_move_piece_type][last_move_dst]
}

impl ThreadData {
    pub fn new() -> Self {
        ThreadData {
            pos: Position::try_from(FEN_START).unwrap(),
            nodes: 0,
            nodes_by_move: [0; u16::MAX as usize],
            root_depth: 1,
            sel_depth: 0,
            stack: from_fn(|_| StackEntry {
                pv: ArrayVec::new_const(),
                both_accs: BothAccumulators::new(),
                hl_activated: [[0; _]; 2],
                is_hl_updated: false,
                raw_eval: None,
            }),
            lmr_table: get_lmr_table(),
            pawns_kings_corr_hist: [[0; CORR_HIST_SIZE]; 2],
            non_pawns_corr_hist: [[[0; CORR_HIST_SIZE]; 2]; 2],
            last_move_corr_hist: [[[0; 64]; 6]; 2],
        }
    }

    pub fn ucinewgame(&mut self) {
        self.pos = Position::try_from(FEN_START).unwrap();
        self.pawns_kings_corr_hist = [[0; CORR_HIST_SIZE]; 2];
        self.non_pawns_corr_hist = [[[0; CORR_HIST_SIZE]; 2]; 2];
        self.last_move_corr_hist = [[[0; 64]; 6]; 2];
    }

    pub fn make_move(&mut self, mov: Option<ChessMove>, ply_before: usize, accs_idx_before: usize) {
        if let Some(mov) = mov {
            self.pos.make_move(mov);
        } else {
            self.pos.make_null_move();
        }

        self.nodes += 1;
        self.sel_depth = self.sel_depth.max(ply_before as u32 + 1);

        let new_stack_entry: &mut StackEntry = self.stack.get_mut_checked_if_debug(ply_before + 1);

        new_stack_entry.pv.clear();
        new_stack_entry.raw_eval = None;

        self.stack
            .get_mut_checked_if_debug(accs_idx_before + 1)
            .is_hl_updated = false;
    }

    // Update principal variation
    pub fn update_pv(&mut self, ply: usize, mov: ChessMove) {
        let child_pv = self.stack.get_checked_if_debug(ply + 1).pv.clone();
        let pv = &mut self.stack.get_mut_checked_if_debug(ply).pv;

        unsafe {
            pv.clear();
            pv.push_unchecked(mov);

            for next_move in child_pv {
                pv.push_unchecked(next_move);
            }
        }
    }

    pub fn ensure_hidden_layer_updated(&mut self, accs_idx: usize) {
        if self.stack.get_checked_if_debug(accs_idx).is_hl_updated {
            return;
        }

        if accs_idx == 0 {
            self.stack[0].both_accs = BothAccumulators::from(&self.pos);
        } else {
            let (left, right) = self.stack.split_at_mut(accs_idx);

            debug_assert!(left.last().unwrap().is_hl_updated);

            unsafe {
                right
                    .first_mut()
                    .debug_unwrap_unchecked()
                    .both_accs
                    .update(&left.last().debug_unwrap_unchecked().both_accs, &self.pos);
            }
        }

        let stack_entry: &mut StackEntry = self.stack.get_mut_checked_if_debug(accs_idx);

        stack_entry.hl_activated = stack_entry.both_accs.activated();
        stack_entry.is_hl_updated = true;
    }

    // Returns static eval corrected
    pub fn static_eval(&mut self, ply: usize, accs_idx: usize) -> i32 {
        debug_assert!(self.stack.get_checked_if_debug(accs_idx).is_hl_updated);

        let raw_eval: i32 = self
            .stack
            .get_checked_if_debug(ply)
            .raw_eval
            .unwrap_or_else(|| {
                value_eval(
                    &self.stack.get_checked_if_debug(accs_idx).hl_activated,
                    self.pos.side_to_move(),
                )
            });

        self.stack.get_mut_checked_if_debug(ply).raw_eval = Some(raw_eval);

        let mut correction: f32 = *(self.pawns_kings_corr()) as f32 * corr_hist_pk_weight();

        correction += *(self.non_pawns_corr(Color::White)) as f32 * corr_hist_nbrqk_weight();
        correction += *(self.non_pawns_corr(Color::Black)) as f32 * corr_hist_nbrqk_weight();

        if let Some(last_move_corr) = self.last_move_corr() {
            correction += *last_move_corr as f32 * corr_hist_last_move_weight();
        }

        correction /= 100.0;

        (raw_eval + (correction.round() as i32)).clamp(-MIN_MATE_SCORE + 1, MIN_MATE_SCORE - 1)
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

    pub fn last_move_corr(&mut self) -> Option<&mut i16> {
        if let Some(last_move) = self.pos.last_move() {
            let stm: Color = self.pos.side_to_move();
            Some(&mut self.last_move_corr_hist[stm][last_move.piece_type()][last_move.dst()])
        } else {
            None
        }
    }
}
