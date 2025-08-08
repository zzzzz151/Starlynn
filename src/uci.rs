use crate::chess::{
    chess_move::ChessMove,
    perft::{perft, perft_split},
    position::Position,
    types::{Color, PieceType, Square},
    util::FEN_START,
};

use crate::search::{bench::DEFAULT_BENCH_DEPTH, bench::bench, eval::evaluate, searcher::Searcher};
use std::num::{NonZeroU32, NonZeroU64};
use std::time::{Duration, Instant};

const OVERHEAD_MS: u64 = 20;

pub fn run_command(command: &str, pos: &mut Position, searcher: &mut Searcher) {
    let command = command.trim();

    let split_ws: Vec<&str> = command
        .split_whitespace()
        .map(|token| token.trim())
        .collect();

    if split_ws.is_empty() {
        return;
    }

    match split_ws[0] {
        // UCI commands
        "uci" => {
            print!("id name Starlynn");
            print!("\nid name zzzzz");
            print!("\noption name Hash type spin default 32 min 1 max 131072");
            print!("\noption name Threads type spin default 1 min 1 max 1");
            println!("\nuciok");
        }
        "ucinewgame" => *pos = Position::try_from(FEN_START).unwrap(),
        "isready" => println!("readyok"),
        "position" => uci_position(&split_ws, pos),
        "go" => uci_go(&split_ws, pos, searcher),
        "quit" => std::process::exit(0),
        // Non-UCI commands
        "display" | "d" | "print" | "show" => pos.display(),
        "perft" => {
            let depth: u32 = split_ws
                .get(1)
                .expect("Expected perft depth argument")
                .parse::<u32>()
                .expect("Error parsing perft depth");

            let start_time = Instant::now();
            let nodes = perft(pos, depth);
            let nps: u64 = nodes * 1000 / (start_time.elapsed().as_millis().max(1) as u64);

            println!("{nodes} nodes {nps} nps");
        }
        "perftsplit" | "splitperft" | "perftdivide" | "divideperft" => {
            let depth: NonZeroU32 = split_ws
                .get(1)
                .unwrap_or_else(|| panic!("Expected {} depth argument", split_ws[0]))
                .parse::<NonZeroU32>()
                .unwrap_or_else(|_| panic!("Error parsing {} depth", split_ws[0]));

            perft_split(pos, depth);
        }
        "bench" => {
            let depth: NonZeroU32 = split_ws
                .get(1)
                .map(|s| s.parse::<NonZeroU32>().expect("Error parsing bench depth"))
                .unwrap_or(DEFAULT_BENCH_DEPTH);

            bench(depth);
        }
        "eval" | "evaluate" | "evaluation" | "raweval" => println!("eval {}", evaluate(pos)),
        _ => {}
    }
}

fn uci_position(tokens: &Vec<&str>, pos: &mut Position) {
    if tokens.len() <= 1 {
        return;
    }

    match tokens[1] {
        "startpos" => *pos = Position::try_from(FEN_START).unwrap(),
        "fen" => {
            let fen: String = tokens
                .iter()
                .skip(2)
                .take_while(|&&s| s != "moves")
                .copied()
                .collect::<Vec<_>>()
                .join(" ");

            *pos = Position::try_from(fen.as_str()).expect("Error parsing FEN");
        }
        _ => panic!("Invalid token after 'position'"),
    }

    if let Some(moves_idx) = tokens.iter().position(|&s| s == "moves") {
        for uci_move in &tokens[(moves_idx + 1)..] {
            let src_square_str: &str = uci_move
                .get(0..2)
                .expect("Error parsing UCI move source square");

            let src: Square =
                Square::try_from(src_square_str).expect("Error parsing UCI move source square");

            let pt: PieceType = pos.at(src).expect("No piece in UCI move source square");

            let mov = ChessMove::from_uci(uci_move, pt).expect("Error parsing UCI move");
            pos.make_move(mov);
        }
    }
}

fn uci_go(tokens: &[&str], pos: &mut Position, searcher: &mut Searcher) {
    let mut max_depth: Option<NonZeroU32> = None;
    let mut max_nodes: Option<NonZeroU64> = None;
    let mut search_time: Option<Duration> = None;

    for pair in tokens[1..].chunks(2) {
        if let &[token1, token2] = pair {
            match token1 {
                "depth" => max_depth = Some(token2.parse().expect("Error parsing depth")),
                "nodes" => max_nodes = Some(token2.parse().expect("Error parsing nodes")),

                token1
                    if token1 == "movetime"
                        || (token1 == "wtime" && pos.side_to_move() == Color::White)
                        || (token1 == "btime" && pos.side_to_move() == Color::Black) =>
                {
                    let mut time_ms: u64 =
                        token2.parse::<i64>().expect("Error parsing time").max(0) as u64;

                    time_ms = time_ms.saturating_sub(OVERHEAD_MS);

                    if token1 != "movetime" {
                        time_ms /= 25;
                    }

                    search_time = Some(Duration::from_millis(time_ms));
                }
                _ => {}
            }
        };
    }

    let best_move: Option<ChessMove> = searcher
        .search(pos, max_depth, max_nodes, search_time, true)
        .0;

    println!(
        "bestmove {}",
        best_move.map_or("0000".to_string(), |mov| mov.to_string())
    );
}
