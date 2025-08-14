use super::move_sorting::{get_scored_moves, remove_best_move};
use crate::chess::{chess_move::ChessMove, position::Position, util::FEN_START};
use crate::nn::{accumulator::BothAccumulators, value_policy_heads::value_eval};
use arrayvec::ArrayVec;
use std::array::from_fn;
use std::num::{NonZeroU32, NonZeroU64};
use std::time::{Duration, Instant};

const MAX_DEPTH: i32 = 100;
const INF: i32 = 30000;
const MIN_MATE_SCORE: i32 = INF - MAX_DEPTH;

pub struct SearchLimits {
    pub start_time: Instant,
    pub max_depth: Option<NonZeroU32>,
    pub max_nodes: Option<NonZeroU64>,
    pub max_duration: Option<Duration>,
    pub max_duration_hit: bool,
}

pub struct ThreadData {
    pub pos: Position,
    nodes: u64,
    root_depth: i32,
    sel_depth: u32,
    pv_table: [ArrayVec<ChessMove, { MAX_DEPTH as usize + 1 }>; MAX_DEPTH as usize + 1],
    accs_stack: [BothAccumulators; MAX_DEPTH as usize + 1],
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
}

// Returns best move and nodes
pub fn search(
    limits: &mut SearchLimits,
    td: &mut ThreadData,
    print_info: bool,
) -> (Option<ChessMove>, u64) {
    assert!(!limits.max_duration_hit);

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
        let elapsed = limits.start_time.elapsed();

        if depth > 1
            && let Some(max_duration) = limits.max_duration
            && elapsed >= max_duration
        {
            break;
        }

        best_move = td.pv_table[0].first().copied();
        assert!(best_move.is_some(), "Expected move");

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

        if let Some(max_nodes) = limits.max_nodes
            && td.nodes >= max_nodes.get()
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
    if limits.max_duration_hit {
        return 0;
    }

    if td.root_depth > 1
        && td.nodes % 1024 == 0
        && let Some(max_duration) = limits.max_duration
        && limits.start_time.elapsed() >= max_duration
    {
        limits.max_duration_hit = true;
        return 0;
    }

    td.sel_depth = td.sel_depth.max(ply);

    if ply > 0 && td.pos.plies_since_pawn_or_capture() >= 100 {
        if td.pos.in_check() && td.pos.legal_moves().is_empty() {
            return -INF + (ply as i32);
        } else {
            return 0;
        }
    }

    if ply > 0 && (td.pos.is_insufficient_material() || td.pos.is_repetition()) {
        return 0;
    }

    let legal_moves = td.pos.legal_moves();

    // Checkmate or stalemate?
    if legal_moves.is_empty() {
        if td.pos.in_check() {
            return -INF + (ply as i32);
        } else {
            return 0;
        }
    }

    let (left, right) = td.accs_stack.split_at_mut(accs_idx);
    debug_assert!(!right.is_empty());
    let accs: &mut BothAccumulators = unsafe { right.first_mut().unwrap_unchecked() };

    if !accs.is_unactivated_updated {
        debug_assert!(!left.is_empty());
        let prev_accs: &BothAccumulators = unsafe { left.last().unwrap_unchecked() };
        accs.update(prev_accs, &td.pos);
    }

    if depth <= 0 || (ply as i32) >= MAX_DEPTH {
        return value_eval(accs, td.pos.side_to_move());
    }

    let mut scored_moves = get_scored_moves(&td.pos, &legal_moves);
    let mut best_score: i32 = -INF;

    while let Some((mov, _)) = remove_best_move(&mut scored_moves) {
        td.pos.make_move(mov);
        td.nodes += 1;

        unsafe {
            td.accs_stack
                .get_unchecked_mut(accs_idx + 1)
                .is_unactivated_updated = false;
        }

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
