use super::limits::SearchLimits;
use super::move_picker::MovePicker;
use super::params::*;
use super::thread_data::ThreadData;
use super::tt::TT;
use super::tt_entry::{Bound, TTEntry};
use crate::GetCheckedIfDebug;
use crate::chess::{chess_move::ChessMove, move_gen::MovesList, position::Position, types::Color};

use crate::nn::{
    accumulator::BothAccumulators, value_policy_heads::ScoredMoves,
    value_policy_heads::get_policy_logits,
};

use arrayvec::ArrayVec;
use std::num::NonZeroU16;

// Returns best move and nodes
pub fn search<const PRINT_INFO: bool>(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    tt: &mut TT,
) -> (Option<ChessMove>, u64) {
    limits.max_duration_hit = false;

    let root_legal_moves: MovesList = td.pos.legal_moves();

    if root_legal_moves.is_empty() {
        return (None, 1);
    }

    // If max nodes is 1 ("go nodes 1"), return best move according to policy net
    if limits
        .max_nodes
        .is_some_and(|max_nodes| max_nodes.get() == 1)
    {
        let mut both_accs = BothAccumulators::from(&td.pos);

        let logits: ScoredMoves =
            get_policy_logits::<false>(&mut both_accs, &td.pos, &td.pos.legal_moves(), None);

        let (best_move, _) = logits
            .into_iter()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
            .expect("Expected a move");

        println!("info depth 0 seldepth 0 nodes 1 nps 1000 time 0 pv {best_move}");

        return (Some(best_move), 1);
    }

    td.nodes = 1;

    for mov in root_legal_moves {
        td.nodes_by_move[NonZeroU16::from(mov).get() as usize] = 0;
    }

    td.stack[0].pv.clear();
    td.stack[0].both_accs.build_accumulators(&td.pos);
    td.stack[0].raw_eval = None;

    let mut score: i32 = 0;

    for depth in 1..=limits
        .max_depth
        .map(|x| x.get() as i32)
        .unwrap_or(MAX_DEPTH)
    {
        td.root_depth = depth;
        td.sel_depth = 0;

        score = if depth <= 4 {
            pvs::<true, true>(limits, td, tt, depth, 0, -INF, INF, 0, None)
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

        // Stop searching if nodes limit hit
        if limits
            .max_nodes
            .is_some_and(|max_nodes| td.nodes >= max_nodes.get())
        {
            break;
        }

        // Stop searching if soft time limit hit
        if limits.max_soft_duration.is_some_and(|mut max_soft_dur| {
            if depth >= 6 {
                // Nodes TM
                // Less/more soft time the bigger/smaller the fraction of nodes spent on best move

                let best_move: ChessMove = *(td.stack[0].pv.first().expect("Expected move"));

                let best_move_nodes: u64 =
                    td.nodes_by_move[NonZeroU16::from(best_move).get() as usize];

                assert!(best_move_nodes <= td.nodes);

                let nodes_fraction: f64 = best_move_nodes as f64 / (td.nodes as f64);
                max_soft_dur = max_soft_dur.mul_f64(2.0 - nodes_fraction * 1.5)
            }

            elapsed >= max_soft_dur
        }) {
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

        score = pvs::<true, true>(limits, td, tt, depth, 0, alpha, beta, 0, None);

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
    singular_move: Option<ChessMove>,
) -> i32 {
    debug_assert!(IS_ROOT == (ply == 0));
    debug_assert!(!IS_ROOT || PV_NODE);
    debug_assert!(depth <= MAX_DEPTH);
    debug_assert!(alpha.abs() <= INF && beta.abs() <= INF);
    debug_assert!(alpha < beta);
    debug_assert!(PV_NODE || beta - alpha == 1);

    // Dive into quiescence search in leaf nodes
    if depth <= 0 {
        return q_search::<PV_NODE>(limits, td, tt, ply, alpha, beta, accs_idx);
    }

    if limits.update_max_duration_hit::<IS_ROOT>(td.root_depth, td.nodes) {
        return 0;
    }

    let mut legal_moves: MovesList = ArrayVec::new_const();

    if let Some(terminal_score) = get_terminal_score::<IS_ROOT>(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    debug_assert!(!legal_moves.is_empty());

    // If singular search has no move to search
    if singular_move.is_some() && legal_moves.len() <= 1 {
        return alpha;
    }

    // Get TT entry data (if any)
    let tt_idx: usize = tt.get_index(td.pos.zobrist_hash());
    let tt_entry: TTEntry = tt[tt_idx];
    let (tt_depth, tt_score, tt_bound, mut tt_move) = tt_entry.get(td.pos.zobrist_hash(), ply);

    // TT cutoff
    if !PV_NODE
        && singular_move.is_none()
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

    let eval: i32 = td.static_eval(ply as usize, accs_idx);

    // Max ply?
    if ply as i32 >= MAX_DEPTH {
        return eval;
    }

    // Node pruning
    if !PV_NODE && !td.pos.in_check() && singular_move.is_none() {
        // RFP (reverse futility pruning)
        if depth <= 7 && beta.abs() < MIN_MATE_SCORE && eval - depth * 75 >= beta {
            return (eval + beta) / 2;
        }

        // Razoring
        if alpha.abs() < MIN_MATE_SCORE && eval + 350 + depth * depth * 275 <= alpha {
            return q_search::<PV_NODE>(limits, td, tt, ply, alpha, beta, accs_idx);
        }

        // NMP (null move pruning)
        if td.pos.last_move().is_some()
            && td.pos.has_nbrq(td.pos.side_to_move())
            && depth >= 3
            && beta.abs() < MIN_MATE_SCORE
            && eval >= beta
        {
            td.make_move(None, ply, accs_idx);

            // Null move search
            let score: i32 = -pvs::<false, false>(
                limits,
                td,
                tt,
                depth - 3 - depth / 3,
                ply + 1,
                -beta,
                -alpha,
                accs_idx,
                None,
            );

            td.pos.undo_move();

            // If null move search failed high with a false mate score,
            // return beta instead of the false mate score
            if score >= beta && score >= MIN_MATE_SCORE {
                return beta;
            }

            // If null move search failed high
            if score >= beta {
                return score;
            }
        }
    }

    // TT move is singular move in singular searches
    // No TT move if it isn't legal
    tt_move = singular_move.or_else(|| tt_move.filter(|tt_mov| legal_moves.contains(tt_mov)));

    // IIR (Internal iterative reduction)
    if PV_NODE && depth >= 4 && tt_move.is_none() {
        depth -= 1;
    }

    let mut move_picker = MovePicker::new(tt_move);
    let mut moves_seen: usize = 0;
    let mut best_score: i32 = -INF;
    let mut bound = Bound::Upper;
    let mut best_move: Option<ChessMove> = None;

    while let Some((mov, _logit)) = {
        let both_accs: &mut BothAccumulators =
            unsafe { &mut td.stack.get_mut_checked_if_debug(accs_idx).both_accs };

        move_picker.next::<false>(&td.pos, &legal_moves, both_accs)
    } {
        // In singular searches, skip TT move
        if Some(mov) == singular_move {
            continue;
        }

        moves_seen += 1;

        let is_quiet_or_underpromo: bool = td.pos.is_quiet_or_underpromotion(mov);
        let is_quiet_or_losing: bool = is_quiet_or_underpromo || !td.pos.see_ge(mov, 0);

        // Move pruning at shallow depths
        if !IS_ROOT && best_score > -MIN_MATE_SCORE && is_quiet_or_losing {
            // LMP (late move pruning)
            if moves_seen as i32 > 3 + depth * depth {
                continue;
            }

            // FP (futility pruning)
            if depth <= 7
                && moves_seen > 2
                && alpha.abs() < MIN_MATE_SCORE
                && eval + 120 + depth * 120 <= alpha
            {
                continue;
            }

            // SEE pruning
            let threshold: i32 = if is_quiet_or_underpromo { -50 } else { -100 };
            if td.pos.has_nbrq(td.pos.side_to_move()) && !td.pos.see_ge(mov, depth * threshold) {
                continue;
            }
        }

        let mut new_depth: i32 = depth - 1;

        // SE (singular extensions)
        if !IS_ROOT
            && Some(mov) == tt_move
            && singular_move.is_none()
            && (ply as i32) < td.root_depth * 2
            && depth >= 6
            && depth - tt_depth <= 3
            && tt_score.abs() < MIN_MATE_SCORE
            && tt_bound.unwrap() != Bound::Upper
        {
            let s_beta: i32 = (tt_score - depth).max(-MIN_MATE_SCORE);

            // Singular search (TT move excluded)
            let s_score: i32 = pvs::<false, false>(
                limits,
                td,
                tt,
                new_depth / 2,
                ply,
                s_beta - 1,
                s_beta,
                accs_idx,
                tt_move,
            );

            // Extend TT move if it is singular (much better than all other moves)
            new_depth += (s_score < s_beta) as i32;

            // Double extension
            new_depth += (!PV_NODE
                && depth < MAX_DEPTH
                && s_score > -MIN_MATE_SCORE
                && s_score + 25 < s_beta) as i32;
        }

        let nodes_before: u64 = td.nodes;

        td.make_move(Some(mov), ply, accs_idx);

        let mut score: i32 = 0;
        let mut do_full_depth_zws: bool = !PV_NODE || moves_seen > 1;

        // LMR (late move reductions)
        if depth >= 2 && moves_seen > 2 + (IS_ROOT as usize) && is_quiet_or_losing {
            let mut reduced_depth: i32 = new_depth;

            // Base reduction
            reduced_depth -= unsafe {
                *td.lmr_table
                    .get_checked_if_debug(depth as usize)
                    .get_checked_if_debug(moves_seen)
            };

            // Reduction adjustments
            reduced_depth += PV_NODE as i32;
            reduced_depth += td.pos.in_check() as i32;
            reduced_depth = reduced_depth.min(new_depth);

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
                None,
            );

            do_full_depth_zws = reduced_depth < new_depth && score > alpha;
        }

        // Full depth, zero window search
        if do_full_depth_zws {
            score = -pvs::<false, false>(
                limits,
                td,
                tt,
                new_depth,
                ply + 1,
                -alpha - 1,
                -alpha,
                accs_idx + 1,
                None,
            );
        }

        // Full depth, full window search
        if PV_NODE && (moves_seen == 1 || score > alpha) {
            score = -pvs::<false, true>(
                limits,
                td,
                tt,
                new_depth,
                ply + 1,
                -beta,
                -alpha,
                accs_idx + 1,
                None,
            );
        }

        td.pos.undo_move();

        if IS_ROOT {
            td.nodes_by_move[NonZeroU16::from(mov).get() as usize] += td.nodes - nodes_before;
        }

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

        // Update principal variation
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

    // Update TT entry
    tt[tt_idx].update(
        td.pos.zobrist_hash(),
        depth as u8,
        best_score as i16,
        ply,
        bound,
        best_move,
    );

    // Update correction histories
    if singular_move.is_none()
        && best_score.abs() < MIN_MATE_SCORE
        && best_move.is_none_or(|mov| td.pos.is_quiet_or_underpromotion(mov))
        && (bound != Bound::Lower || best_score > eval)
        && (bound != Bound::Upper || best_score < eval)
    {
        let mut bonus: i32 = (best_score - eval) * depth;
        bonus = bonus.clamp(-HISTORY_MAX, HISTORY_MAX);

        update_history(td.pawns_kings_corr(), bonus);
        update_history(td.non_pawns_corr(Color::White), bonus);
        update_history(td.non_pawns_corr(Color::Black), bonus);

        if let Some(last_move_corr) = td.last_move_corr() {
            update_history(last_move_corr, bonus);
        }
    }

    best_score
}

// Quiescence search
fn q_search<const PV_NODE: bool>(
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

    let mut legal_moves: MovesList = ArrayVec::new_const();

    if let Some(terminal_score) = get_terminal_score::<false>(&td.pos, ply, &mut legal_moves) {
        return terminal_score;
    }

    debug_assert!(!legal_moves.is_empty());

    // Get TT entry data (if any)
    let tt_idx: usize = tt.get_index(td.pos.zobrist_hash());
    let tt_entry: TTEntry = tt[tt_idx];
    let (_, tt_score, tt_bound, mut tt_move) = tt_entry.get(td.pos.zobrist_hash(), ply);

    // TT cutoff
    if !PV_NODE && let Some(tt_bound) = tt_bound {
        #[allow(clippy::collapsible_if)]
        if tt_bound == Bound::Exact
            || (tt_bound == Bound::Upper && tt_score <= alpha)
            || (tt_bound == Bound::Lower && tt_score >= beta)
        {
            return tt_score;
        }
    }

    let eval: i32 = td.static_eval(ply as usize, accs_idx);

    // Max ply or static eval fails high?
    if ply as i32 >= MAX_DEPTH || eval >= beta {
        return eval;
    }

    alpha = alpha.max(eval);

    // No TT move if it is quiet, underpromotion or illegal
    tt_move = tt_move.filter(|tt_mov| {
        !td.pos.is_quiet_or_underpromotion(*tt_mov) && legal_moves.contains(tt_mov)
    });

    let mut move_picker = MovePicker::new(tt_move);
    let mut best_score: i32 = eval;
    let mut bound = Bound::Upper;
    let mut best_move: Option<ChessMove> = None;

    while let Some((mov, _logit)) = {
        let both_accs: &mut BothAccumulators =
            unsafe { &mut td.stack.get_mut_checked_if_debug(accs_idx).both_accs };

        move_picker.next::<true>(&td.pos, &legal_moves, both_accs)
    } {
        td.make_move(Some(mov), ply, accs_idx);

        let score: i32 = -q_search::<PV_NODE>(limits, td, tt, ply + 1, -beta, -alpha, accs_idx + 1);

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
        best_move = Some(mov);

        // Fail high?
        if score >= beta {
            bound = Bound::Lower;
            break;
        }
    }

    debug_assert!(best_score.abs() < INF);

    // Update TT entry
    tt[tt_idx].update(
        td.pos.zobrist_hash(),
        0,
        best_score as i16,
        ply,
        bound,
        best_move,
    );

    best_score
}

fn get_terminal_score<const IS_ROOT: bool>(
    pos: &Position,
    ply: u32,
    legal_moves: &mut MovesList,
) -> Option<i32> {
    debug_assert!(IS_ROOT == (ply == 0));

    if IS_ROOT {
        *legal_moves = pos.legal_moves();
        return None;
    }

    // 50 moves rule
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

fn update_history(value: &mut i16, bonus: i32) {
    debug_assert!(bonus.abs() <= HISTORY_MAX);
    let delta: i32 = bonus - bonus.abs() * (*value as i32) / HISTORY_MAX;
    *value += delta as i16;
}
