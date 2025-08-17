use super::limits::SearchLimits;
use super::params::*;
use super::thread_data::ThreadData;
use super::tt::TT;
use super::tt_entry::{Bound, TTEntry};
use crate::GetCheckedIfDebug;
use crate::chess::{chess_move::ChessMove, position::Position};
use crate::nn::{accumulator::BothAccumulators, value_policy_heads::get_policy_logits};
use arrayvec::ArrayVec;
use debug_unwraps::DebugUnwrapExt;
use std::cmp::Ordering;

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
    td.stack[0].pv.clear();
    td.stack[0].both_accs = BothAccumulators::from(&td.pos);

    let mut score: i32 = 0;

    for depth in 1..=limits
        .max_depth
        .map(|x| x.get() as i32)
        .unwrap_or(MAX_DEPTH)
    {
        td.root_depth = depth;
        td.sel_depth = 0;

        score = if depth <= 4 {
            pvs::<true, true>(limits, td, tt, depth, 0, -INF, INF, 0)
        } else {
            aspiration_windows(limits, td, tt, depth, score)
        };

        if limits.max_duration_hit {
            break;
        }

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

            let pv_str: String = td.stack[0]
                .pv
                .iter()
                .map(|mov| mov.to_string())
                .collect::<Vec<_>>()
                .join(" ");

            println!(
                "info depth {} seldepth {} nodes {} nps {} time {} score {} pv {}",
                depth,
                td.sel_depth,
                td.nodes,
                td.nodes * 1000 / (elapsed.as_millis().max(1) as u64),
                elapsed.as_millis(),
                score_str,
                pv_str
            );
        }

        if limits
            .max_nodes
            .is_some_and(|max_nodes| td.nodes >= max_nodes.get())
        {
            break;
        }
    }

    let best_move: ChessMove = *(td.stack[0].pv.first().expect("Expected move"));
    (Some(best_move), td.nodes)
}

fn aspiration_windows(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    tt: &mut TT,
    depth: i32,
    mut score: i32,
) -> i32 {
    debug_assert!(depth > 1);

    let mut delta: i32 = 16;

    let (mut alpha, mut beta) = if score <= -MIN_MATE_SCORE {
        (-INF, -MIN_MATE_SCORE + 1)
    } else if score >= MIN_MATE_SCORE {
        (MIN_MATE_SCORE - 1, INF)
    } else {
        (score - delta, score + delta)
    };

    loop {
        debug_assert!(alpha < MIN_MATE_SCORE);
        debug_assert!(beta > -MIN_MATE_SCORE);

        if alpha <= -MIN_MATE_SCORE {
            alpha = -INF;
        }

        if beta >= MIN_MATE_SCORE {
            beta = INF;
        }

        score = pvs::<true, true>(limits, td, tt, depth, 0, alpha, beta, 0);

        if limits.max_duration_hit {
            return 0;
        }

        // Fail low?
        if score <= alpha {
            alpha -= delta;

            if alpha.abs() < MIN_MATE_SCORE && beta.abs() < MIN_MATE_SCORE {
                beta = (alpha + beta) / 2;
            }
        }
        // Fail high?
        else if score >= beta {
            beta += delta;
        }
        // Else, score is exact
        else {
            return score;
        }

        delta += delta / 2;
    }
}

// Principal variation search
#[allow(clippy::too_many_arguments)]
fn pvs<const IS_ROOT: bool, const PV_NODE: bool>(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    tt: &mut TT,
    mut depth: i32,
    ply: u32,
    mut alpha: i32,
    beta: i32,
    accs_idx: usize,
) -> i32 {
    debug_assert!(IS_ROOT == (ply == 0));
    debug_assert!(!IS_ROOT || PV_NODE);
    debug_assert!(alpha.abs() <= INF && beta.abs() <= INF);
    debug_assert!(alpha < beta);
    debug_assert!(PV_NODE || beta - alpha == 1);

    if depth <= 0 {
        return q_search(limits, td, tt, ply, alpha, beta, accs_idx);
    }

    if limits.update_max_duration_hit::<IS_ROOT>(td.root_depth, td.nodes) {
        return 0;
    }

    let mut legal_moves: ArrayVec<ChessMove, 256> = ArrayVec::new();

    if let Some(terminal_score) = get_terminal_score::<IS_ROOT>(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    debug_assert!(!legal_moves.is_empty());

    let tt_idx: usize = tt.get_index(td.pos.zobrist_hash());
    let tt_entry: TTEntry = tt[tt_idx];
    let (tt_depth, tt_score, tt_bound, mut tt_move) = tt_entry.get(td.pos.zobrist_hash(), ply);

    // TT cutoff
    if !PV_NODE
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

    td.update_both_accs(accs_idx);

    if ply as i32 >= MAX_DEPTH {
        return td.static_eval(accs_idx);
    }

    // Node pruning
    if !PV_NODE && !td.pos.in_check() && beta.abs() < MIN_MATE_SCORE {
        let eval: i32 = td.static_eval(accs_idx);

        // RFP (Reverse futility pruning)
        if depth <= 7 && eval - depth * 75 >= beta {
            return (eval + beta) / 2;
        }

        // NMP (Null move pruning)
        if td.pos.last_move().is_some()
            && td.pos.has_nbrq(td.pos.side_to_move())
            && depth >= 3
            && eval >= beta
        {
            td.make_move(None, ply, accs_idx);

            let score: i32 = -pvs::<false, false>(
                limits,
                td,
                tt,
                depth - 3 - depth / 3,
                ply + 1,
                -beta,
                -alpha,
                accs_idx,
            );

            td.pos.undo_move();

            if score >= beta && score >= MIN_MATE_SCORE {
                return beta;
            }

            if score >= beta {
                return score;
            }
        }
    }

    // No TT move if it isn't legal
    tt_move = tt_move.filter(|tt_mov| legal_moves.contains(tt_mov));

    // IIR (Internal iterative reduction)
    if depth >= 4 && tt_move.is_none() {
        depth -= 1;
    }

    let mut moves_seen: usize = 0;
    let mut logits: ArrayVec<(ChessMove, f32), 256> = ArrayVec::new();
    let mut is_move_quiet_or_losing: bool = false;
    let mut best_score: i32 = -INF;
    let mut bound = Bound::Upper;
    let mut best_move: Option<ChessMove> = None;

    loop {
        // LMP (Late move pruning)
        if !IS_ROOT
            && best_score > -MIN_MATE_SCORE
            && is_move_quiet_or_losing
            && moves_seen as i32 > 3 + depth * depth
        {
            break;
        }

        let both_accs: &mut BothAccumulators =
            unsafe { &mut td.stack.get_mut_checked_if_debug(accs_idx).both_accs };

        let mov: ChessMove = match next_best_move::<false>(
            &td.pos,
            &legal_moves,
            &mut moves_seen,
            tt_move,
            both_accs,
            &mut logits,
        ) {
            Some(mov) => mov,
            None => break,
        };

        let is_quiet_or_underpromo: bool = !td.pos.is_noisy_not_underpromotion(mov);
        is_move_quiet_or_losing = is_quiet_or_underpromo || !td.pos.see_ge(mov, 0);

        // SEE pruning
        if !IS_ROOT
            && best_score > -MIN_MATE_SCORE
            && is_move_quiet_or_losing
            && td.pos.has_nbrq(td.pos.side_to_move())
            && !td
                .pos
                .see_ge(mov, depth * [-100, -50][is_quiet_or_underpromo as usize])
        {
            continue;
        }

        td.make_move(Some(mov), ply, accs_idx);

        let mut score: i32 = 0;
        let mut do_full_depth_zws: bool = !PV_NODE || moves_seen > 1;

        // LMR (Late move reductions)
        if depth >= 2 && moves_seen > 2 + (IS_ROOT as usize) && is_move_quiet_or_losing {
            let mut reduced_depth: i32 = depth - 1;

            reduced_depth -= unsafe {
                *td.lmr_table
                    .get_checked_if_debug(depth as usize)
                    .get_checked_if_debug(moves_seen)
            };

            reduced_depth += PV_NODE as i32;
            reduced_depth += td.pos.in_check() as i32;
            reduced_depth = reduced_depth.min(depth - 1);

            // Reduced depth, zero window search
            score = -pvs::<false, false>(
                limits,
                td,
                tt,
                reduced_depth,
                ply + 1,
                -alpha - 1,
                -alpha,
                accs_idx + 1,
            );

            do_full_depth_zws = reduced_depth < depth - 1 && score > alpha;
        }

        // Full depth, zero window search
        if do_full_depth_zws {
            score = -pvs::<false, false>(
                limits,
                td,
                tt,
                depth - 1,
                ply + 1,
                -alpha - 1,
                -alpha,
                accs_idx + 1,
            );
        }

        // Full depth, full window search
        if PV_NODE && (moves_seen == 1 || score > alpha) {
            score = -pvs::<false, true>(
                limits,
                td,
                tt,
                depth - 1,
                ply + 1,
                -beta,
                -alpha,
                accs_idx + 1,
            );
        }

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

        if PV_NODE {
            td.update_pv(ply as usize, mov);
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

    if limits.update_max_duration_hit::<false>(td.root_depth, td.nodes) {
        return 0;
    }

    let mut legal_moves: ArrayVec<ChessMove, 256> = ArrayVec::new();

    if let Some(terminal_score) = get_terminal_score::<false>(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    debug_assert!(!legal_moves.is_empty());

    let eval: i32 = td.static_eval(accs_idx);

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
        unsafe { &mut td.stack.get_mut_checked_if_debug(accs_idx).both_accs },
        &mut logits,
    ) {
        td.make_move(Some(mov), ply, accs_idx);

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

fn get_terminal_score<const IS_ROOT: bool>(
    pos: &Position,
    ply: u32,
    legal_moves: &mut ArrayVec<ChessMove, 256>,
) -> Option<i32> {
    debug_assert!(IS_ROOT == (ply == 0));

    if IS_ROOT {
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
                unsafe { cmp.debug_unwrap_unchecked() }
            })
    {
        *legal_moves_seen += 1;
        Some(scored_moves.swap_remove(best_idx).0)
    } else {
        None
    }
}
