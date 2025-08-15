use super::limits::SearchLimits;
use super::params::*;
use super::thread_data::{AccumulatorsStackExt, ThreadData};
use super::tt::TT;
use super::tt_entry::{Bound, TTEntry};
use crate::chess::{chess_move::ChessMove, position::Position};
use arrayvec::ArrayVec;
use std::cmp::Ordering;

use crate::nn::{
    accumulator::BothAccumulators,
    value_policy_heads::{get_policy_logits, value_eval},
};

// Returns best move and nodes
pub fn search<const PRINT_INFO: bool>(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    tt: &mut TT,
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

        let score: i32 = negamax(limits, td, tt, depth, 0, -INF, INF, 0);

        if limits.max_duration_hit {
            break;
        }

        best_move = td.pv_table[0].first().copied();
        assert!(best_move.is_some(), "Expected move");

        let elapsed = limits.start_time.elapsed();

        if PRINT_INFO {
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

#[allow(clippy::too_many_arguments)]
fn negamax(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    tt: &mut TT,
    depth: i32,
    ply: u32,
    mut alpha: i32,
    beta: i32,
    accs_idx: usize,
) -> i32 {
    debug_assert!(alpha.abs() <= INF && beta.abs() <= INF);
    debug_assert!(alpha < beta);

    if depth <= 0 {
        return q_search(limits, td, tt, ply, alpha, beta, accs_idx);
    }

    if limits.update_max_duration_hit(ply == 0, td.root_depth, td.nodes) {
        return 0;
    }

    let mut legal_moves: ArrayVec<ChessMove, 256> = ArrayVec::new();

    if let Some(terminal_score) = get_terminal_score(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    debug_assert!(!legal_moves.is_empty());

    let tt_idx: usize = tt.get_index(td.pos.zobrist_hash());
    let tt_entry: TTEntry = tt[tt_idx];
    let (tt_depth, tt_score, tt_bound, mut tt_move) = tt_entry.get(td.pos.zobrist_hash(), ply);

    // TT cutoff
    if ply > 0
        && let Some(tt_bound) = tt_bound
        && tt_depth >= depth
    {
        #[allow(clippy::collapsible_if)]
        if tt_bound == Bound::Exact
            || (tt_bound == Bound::Upper && tt_score <= alpha)
            || (tt_bound == Bound::Lower && tt_score >= beta)
        {
            return tt_score;
        }
    }

    let both_accs: &mut BothAccumulators = td.accs_stack.updated_accs(&td.pos, accs_idx);

    if ply as i32 >= MAX_DEPTH {
        return value_eval(both_accs, td.pos.side_to_move());
    }

    // No TT move if it isn't legal
    tt_move = tt_move.filter(|tt_mov| legal_moves.contains(tt_mov));

    let mut moves_seen: usize = 0;
    let mut logits: ArrayVec<(ChessMove, f32), 256> = ArrayVec::new();
    let mut best_score: i32 = -INF;
    let mut bound = Bound::Upper;
    let mut best_move: Option<ChessMove> = None;

    while let Some(mov) = next_best_move::<false>(
        &td.pos,
        &legal_moves,
        &mut moves_seen,
        tt_move,
        td.accs_stack.updated_accs(&td.pos, accs_idx),
        &mut logits,
    ) {
        td.make_move(mov, ply, accs_idx);

        let score: i32 = -negamax(
            limits,
            td,
            tt,
            depth - 1,
            ply + 1,
            -beta,
            -alpha,
            accs_idx + 1,
        );

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
        bound = Bound::Exact;
        best_move = Some(mov);

        if ply == 0 {
            td.pv_table[0].clear();
            td.pv_table[0].push(mov);
        }

        // Fail high?
        if score >= beta {
            bound = Bound::Lower;
            break;
        }
    }

    debug_assert!(best_score.abs() < INF);

    tt[tt_idx].update(
        td.pos.zobrist_hash(),
        depth as u8,
        best_score as i16,
        ply,
        bound,
        best_move,
    );

    best_score
}

// Quiescence search
fn q_search(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    tt: &mut TT,
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

    debug_assert!(!legal_moves.is_empty());

    let both_accs: &mut BothAccumulators = td.accs_stack.updated_accs(&td.pos, accs_idx);
    let eval: i32 = value_eval(both_accs, td.pos.side_to_move());

    if ply as i32 >= MAX_DEPTH || eval >= beta {
        return eval;
    }

    alpha = alpha.max(eval);

    let tt_idx: usize = tt.get_index(td.pos.zobrist_hash());
    let tt_entry: TTEntry = tt[tt_idx];
    let (_, _, _, mut tt_move) = tt_entry.get(td.pos.zobrist_hash(), ply);

    // No TT move if it is quiet, underpromotion or illegal
    tt_move = tt_move.filter(|tt_mov| {
        td.pos.is_noisy_not_underpromotion(*tt_mov) && legal_moves.contains(tt_mov)
    });

    let mut moves_seen: usize = 0;
    let mut logits: ArrayVec<(ChessMove, f32), 256> = ArrayVec::new();
    let mut best_score: i32 = eval;

    while let Some(mov) = next_best_move::<true>(
        &td.pos,
        &legal_moves,
        &mut moves_seen,
        tt_move,
        td.accs_stack.updated_accs(&td.pos, accs_idx),
        &mut logits,
    ) {
        td.make_move(mov, ply, accs_idx);

        let score: i32 = -q_search(limits, td, tt, ply + 1, -beta, -alpha, accs_idx + 1);

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

fn next_best_move<const Q_SEARCH: bool>(
    pos: &Position,
    legal_moves: &ArrayVec<ChessMove, 256>,
    legal_moves_seen: &mut usize,
    tt_move: Option<ChessMove>,
    both_accs: &mut BothAccumulators,
    scored_moves: &mut ArrayVec<(ChessMove, f32), 256>,
) -> Option<ChessMove> {
    debug_assert!(!legal_moves.is_empty());
    debug_assert!(tt_move.is_none() || legal_moves.contains(&tt_move.unwrap()));

    if *legal_moves_seen == 0 && tt_move.is_some() {
        *legal_moves_seen += 1;
        return tt_move;
    }

    if *legal_moves_seen == (tt_move.is_some() as usize) {
        *scored_moves = get_policy_logits::<Q_SEARCH>(both_accs, pos, legal_moves, tt_move);
    }

    if let Some((best_idx, _)) =
        scored_moves
            .iter()
            .enumerate()
            .max_by(|(_, (_, logit1)), (_, (_, logit2))| {
                let cmp: Option<Ordering> = logit1.partial_cmp(logit2);
                debug_assert!(cmp.is_some());
                unsafe { cmp.unwrap_unchecked() }
            })
    {
        *legal_moves_seen += 1;
        Some(scored_moves.swap_remove(best_idx).0)
    } else {
        None
    }
}
