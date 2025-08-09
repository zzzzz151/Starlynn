use super::eval::evaluate;
use super::move_sorting::{get_scored_moves, remove_best_move};
use crate::chess::{chess_move::ChessMove, position::Position, util::FEN_START};
use arrayvec::ArrayVec;
use std::array::from_fn;
use std::num::{NonZeroU32, NonZeroU64};
use std::time::{Duration, Instant};

const MAX_DEPTH: i32 = 100;
const INF: i32 = 30000;
const MIN_MATE_SCORE: i32 = INF - MAX_DEPTH;

pub struct Searcher {
    pos: Position,
    nodes: u64,
    start_time: Instant,
    max_duration: Option<Duration>,
    max_duration_hit: bool,
    root_depth: i32,
    sel_depth: u32,
    pv_table: [ArrayVec<ChessMove, { MAX_DEPTH as usize + 1 }>; MAX_DEPTH as usize + 1],
}

impl Searcher {
    pub fn new() -> Self {
        Searcher {
            pos: Position::try_from(FEN_START).unwrap(),
            nodes: 0,
            start_time: Instant::now(),
            max_duration: None,
            max_duration_hit: false,
            root_depth: 1,
            sel_depth: 0,
            pv_table: from_fn(|_| ArrayVec::new()),
        }
    }

    // Returns best move and nodes count
    pub fn search(
        &mut self,
        pos: &Position,
        max_depth: Option<NonZeroU32>,
        max_nodes: Option<NonZeroU64>,
        max_duration: Option<Duration>,
        print_info: bool,
    ) -> (Option<ChessMove>, u64) {
        self.start_time = Instant::now();

        if pos.legal_moves().is_empty() {
            return (None, 1);
        }

        self.pos = pos.clone();
        self.nodes = 1;
        self.max_duration = max_duration;
        self.max_duration_hit = false;
        self.pv_table[0].clear();

        let mut best_move: Option<ChessMove> = None;

        for depth in 1..=max_depth.map(|x| x.get() as i32).unwrap_or(MAX_DEPTH) {
            self.root_depth = depth;
            self.sel_depth = 0;

            let score: i32 = self.negamax(depth, 0, -INF, INF);
            let elapsed = self.start_time.elapsed();

            if depth > 1
                && let Some(max_duration) = self.max_duration
                && elapsed >= max_duration
            {
                break;
            }

            best_move = self.pv_table[0].first().copied();
            assert!(best_move.is_some(), "Expected move");

            if print_info {
                let score_str = if score.abs() < MIN_MATE_SCORE {
                    format!("cp {score}")
                } else {
                    let plies_to_mate: i32 = INF - score.abs();
                    let full_moves_to_mate: i32 = (plies_to_mate + 1) / 2;
                    let sign = if score > 0 { 1 } else { -1 };
                    format!("mate {}", full_moves_to_mate * sign)
                };

                println!(
                    "info depth {} seldepth {} nodes {} nps {} time {} score {} pv {}",
                    depth,
                    self.sel_depth,
                    self.nodes,
                    self.nodes * 1000 / (elapsed.as_millis().max(1) as u64),
                    elapsed.as_millis(),
                    score_str,
                    unsafe { best_move.unwrap_unchecked() }
                );
            }

            if let Some(max_nodes) = max_nodes
                && self.nodes >= max_nodes.get()
            {
                break;
            }
        }

        (best_move, self.nodes)
    }

    fn negamax(&mut self, depth: i32, ply: u32, mut alpha: i32, beta: i32) -> i32 {
        if self.max_duration_hit {
            debug_assert!(self.root_depth > 1);
            return 0;
        }

        if self.root_depth > 1
            && self.nodes % 1024 == 0
            && let Some(max_duration) = self.max_duration
            && self.start_time.elapsed() >= max_duration
        {
            self.max_duration_hit = true;
            return 0;
        }

        self.sel_depth = self.sel_depth.max(ply);

        if ply > 0 && self.pos.plies_since_pawn_or_capture() >= 100 {
            if self.pos.in_check() && self.pos.legal_moves().is_empty() {
                return -INF + (ply as i32);
            } else {
                return 0;
            }
        }

        if ply > 0 && (self.pos.is_insufficient_material() || self.pos.is_repetition()) {
            return 0;
        }

        let legal_moves = self.pos.legal_moves();

        // Checkmate or stalemate?
        if legal_moves.is_empty() {
            if self.pos.in_check() {
                return -INF + (ply as i32);
            } else {
                return 0;
            }
        }

        if depth <= 0 || (ply as i32) >= MAX_DEPTH {
            return evaluate(&self.pos);
        }

        let mut scored_moves = get_scored_moves(&self.pos, &legal_moves);
        let mut best_score: i32 = -INF;

        while let Some(mov) = remove_best_move(&mut scored_moves) {
            self.pos.make_move(mov);
            self.nodes += 1;

            let score: i32 = -self.negamax(depth - 1, ply + 1, -beta, -alpha);

            self.pos.undo_move();

            if self.max_duration_hit {
                return 0;
            }

            best_score = best_score.max(score);

            // Fail low?
            if score <= alpha {
                continue;
            }

            alpha = score;

            if ply == 0 {
                self.pv_table[0].clear();
                self.pv_table[0].push(mov);
            }

            // Fail high?
            if score >= beta {
                break;
            }
        }

        debug_assert!(best_score.abs() < INF);
        best_score
    }
}
