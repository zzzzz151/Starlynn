use crate::chess::{chess_move::ChessMove, move_gen::MovesList, position::Position};
use arrayvec::ArrayVec;
use debug_unwraps::DebugUnwrapExt;
use std::cmp::Ordering;

use crate::nn::{
    accumulator::BothAccumulators,
    value_policy_heads::{ScoredMoves, get_policy_logits},
};

pub struct MovePicker {
    tt_move: Option<ChessMove>,
    moves_returned: usize,
    scored_moves: ScoredMoves,
}

impl MovePicker {
    pub const fn new(tt_move: Option<ChessMove>) -> Self {
        MovePicker {
            tt_move,
            moves_returned: 0,
            scored_moves: ArrayVec::new_const(),
        }
    }

    pub fn next<const Q_SEARCH: bool>(
        &mut self,
        pos: &Position,
        legal_moves: &MovesList,
        both_accs: &mut BothAccumulators,
    ) -> Option<(ChessMove, Option<f32>)> {
        // Return TT move?
        if self.moves_returned == 0
            && let Some(tt_move) = self.tt_move
        {
            self.moves_returned += 1;
            return Some((tt_move, None));
        }

        // Compute logits if not already computed
        if self.moves_returned == (self.tt_move.is_some() as usize) {
            self.scored_moves =
                get_policy_logits::<Q_SEARCH>(both_accs, pos, legal_moves, self.tt_move);
        }

        // Find move with highest logit, remove it from the scored_moves list, and return it
        if let Some((best_idx, _)) =
            self.scored_moves
                .iter()
                .enumerate()
                .max_by(|(_, (_, logit1)), (_, (_, logit2))| {
                    let cmp: Option<Ordering> = logit1.partial_cmp(logit2);
                    unsafe { cmp.debug_unwrap_unchecked() }
                })
        {
            self.moves_returned += 1;
            let (best_move, logit): (ChessMove, f32) = self.scored_moves.swap_remove(best_idx);
            Some((best_move, Some(logit)))
        // Else if scored_moves list is empty
        } else {
            None
        }
    }
}
