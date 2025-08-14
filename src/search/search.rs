use super::limits::SearchLimits;
use super::params::*;
use super::thread_data::{AccumulatorsStackExt, ThreadData};
use crate::chess::{chess_move::ChessMove, position::Position};
use arrayvec::ArrayVec;

use crate::nn::{
    accumulator::BothAccumulators,
    value_policy_heads::{get_policy_logits, value_eval},
};

// Returns best move and nodes
pub fn search(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    print_info: bool,
) -> (Option<ChessMove>, u64) {
    limits.max_duration_hit = false;

    if td.pos.legal_moves().is_empty() {
        return (None, 1);
    }

    td.nodes = 1;
    td.pv_table[0].clear();
    td.accs_stack[0] = BothAccumulators::from(&td.pos);

    let mut best_move: Option<ChessMove> = None;

    for depth in 1..=limits
        .max_depth
        .map(|x| x.get() as i32)
        .unwrap_or(MAX_DEPTH)
    {
        td.root_depth = depth;
        td.sel_depth = 0;

        let score: i32 = negamax(limits, td, depth, 0, -INF, INF, 0);

        if limits.max_duration_hit {
            break;
        }

        best_move = td.pv_table[0].first().copied();
        assert!(best_move.is_some(), "Expected move");

        let elapsed = limits.start_time.elapsed();

        if print_info {
            let score_str = if score.abs() < MIN_MATE_SCORE {
                format!("cp {score}")
            } else {
                let plies_to_mate: i32 = INF - score.abs();
                let full_moves_to_mate: i32 = (plies_to_mate + 1) / 2;
                let sign: i32 = if score > 0 { 1 } else { -1 };
                format!("mate {}", full_moves_to_mate * sign)
            };

            println!(
                "info depth {} seldepth {} nodes {} nps {} time {} score {} pv {}",
                depth,
                td.sel_depth,
                td.nodes,
                td.nodes * 1000 / (elapsed.as_millis().max(1) as u64),
                elapsed.as_millis(),
                score_str,
                unsafe { best_move.unwrap_unchecked() }
            );
        }

        if limits
            .max_nodes
            .is_some_and(|max_nodes| td.nodes >= max_nodes.get())
        {
            break;
        }
    }

    (best_move, td.nodes)
}

fn negamax(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    depth: i32,
    ply: u32,
    mut alpha: i32,
    beta: i32,
    accs_idx: usize,
) -> i32 {
    debug_assert!(alpha.abs() <= INF && beta.abs() <= INF);
    debug_assert!(alpha < beta);

    if depth <= 0 {
        return q_search(limits, td, ply, alpha, beta, accs_idx);
    }

    if limits.update_max_duration_hit(ply == 0, td.root_depth, td.nodes) {
        return 0;
    }

    let mut legal_moves: ArrayVec<ChessMove, 256> = ArrayVec::new();

    if let Some(terminal_score) = get_terminal_score(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    let accs: &mut BothAccumulators = td.accs_stack.updated_accs(&td.pos, accs_idx);

    if ply as i32 >= MAX_DEPTH {
        return value_eval(accs, td.pos.side_to_move());
    }

    let mut scored_moves = get_policy_logits::<false>(accs, &td.pos, &legal_moves);
    let mut best_score: i32 = -INF;

    while let Some((mov, _)) = remove_best_move(&mut scored_moves) {
        td.make_move(mov, ply, accs_idx);

        let score: i32 = -negamax(limits, td, depth - 1, ply + 1, -beta, -alpha, accs_idx + 1);

        td.pos.undo_move();

        if limits.max_duration_hit {
            return 0;
        }

        best_score = best_score.max(score);

        // Fail low?
        if score <= alpha {
            continue;
        }

        alpha = score;

        if ply == 0 {
            td.pv_table[0].clear();
            td.pv_table[0].push(mov);
        }

        // Fail high?
        if score >= beta {
            break;
        }
    }

    debug_assert!(best_score.abs() < INF);
    best_score
}

// Quiescence search
fn q_search(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    ply: u32,
    mut alpha: i32,
    beta: i32,
    accs_idx: usize,
) -> i32 {
    debug_assert!(ply > 0);
    debug_assert!(alpha.abs() <= INF && beta.abs() <= INF);
    debug_assert!(alpha < beta);

    if limits.update_max_duration_hit(ply == 0, td.root_depth, td.nodes) {
        return 0;
    }

    let mut legal_moves: ArrayVec<ChessMove, 256> = ArrayVec::new();

    if let Some(terminal_score) = get_terminal_score(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    let accs: &mut BothAccumulators = td.accs_stack.updated_accs(&td.pos, accs_idx);
    let eval: i32 = value_eval(accs, td.pos.side_to_move());

    if ply as i32 >= MAX_DEPTH || eval >= beta {
        return eval;
    }

    alpha = alpha.max(eval);

    let mut scored_moves = get_policy_logits::<true>(accs, &td.pos, &legal_moves);
    let mut best_score: i32 = eval;

    while let Some((mov, _)) = remove_best_move(&mut scored_moves) {
        td.make_move(mov, ply, accs_idx);

        let score: i32 = -q_search(limits, td, ply + 1, -beta, -alpha, accs_idx + 1);

        td.pos.undo_move();

        if limits.max_duration_hit {
            return 0;
        }

        best_score = best_score.max(score);

        // Fail low?
        if score <= alpha {
            continue;
        }

        alpha = score;

        // Fail high?
        if score >= beta {
            break;
        }
    }

    debug_assert!(best_score.abs() < INF);
    best_score
}

fn get_terminal_score(
    pos: &Position,
    ply: u32,
    legal_moves: &mut ArrayVec<ChessMove, 256>,
) -> Option<i32> {
    if ply == 0 {
        *legal_moves = pos.legal_moves();
        return None;
    }

    if pos.plies_since_pawn_or_capture() >= 100 {
        if pos.in_check() && pos.legal_moves().is_empty() {
            return Some(-INF + (ply as i32));
        } else {
            return Some(0);
        }
    }

    if pos.is_insufficient_material() || pos.is_repetition() {
        return Some(0);
    }

    *legal_moves = pos.legal_moves();

    // Checkmate or stalemate?
    if legal_moves.is_empty() {
        if pos.in_check() {
            return Some(-INF + (ply as i32));
        } else {
            return Some(0);
        }
    }

    None
}

pub fn remove_best_move<T: Copy + PartialOrd>(
    scored_moves: &mut ArrayVec<(ChessMove, T), 256>,
) -> Option<(ChessMove, T)> {
    if scored_moves.is_empty() {
        return None;
    }

    let mut best_idx: usize = 0;

    for i in 1..scored_moves.len() {
        let best_score: T = unsafe { scored_moves.get_unchecked(best_idx).1 };

        if scored_moves[i].1 > best_score {
            best_idx = i;
        }
    }

    Some(scored_moves.swap_remove(best_idx))
}
