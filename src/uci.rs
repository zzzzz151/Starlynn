use std::num::NonZeroU32;
use std::time::{Duration, Instant};

use crate::chess::{
    chess_move::ChessMove,
    perft::{perft, perft_split},
    position::Position,
    types::{Color, PieceType, Square},
    util::FEN_START,
};

use crate::nn::{accumulator::BothAccumulators, value_policy_heads::*};

use crate::search::{
    bench::{DEFAULT_BENCH_DEPTH, bench},
    limits::{HARD_TIME_PERCENTAGE, SOFT_TIME_PERCENTAGE, SearchLimits},
    search::search,
    thread_data::ThreadData,
    tt::TT,
};

const OVERHEAD_MS: u64 = 20;

pub fn run_command(command: &str, td: &mut ThreadData, tt: &mut TT) {
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
        "setoption" => {
            let name: &str = split_ws
                .iter()
                .position(|&token| token == "name" || token == "Name")
                .and_then(|i| split_ws.get(i + 1))
                .expect("Couldn't get option name");

            let value_str: &str = split_ws
                .iter()
                .position(|&token| token == "value" || token == "Value")
                .and_then(|i| split_ws.get(i + 1))
                .expect("Couldn't get option value");

            match name {
                "Hash" | "hash" => {
                    *tt = TT::new(value_str.parse().expect("Error parsing Hash option value"));
                    tt.print_size::<false>();
                }
                _ => println!("info string Unknown option {name}"),
            }
        }
        "ucinewgame" => {
            td.ucinewgame();
            tt.reset_keep_size();
        }
        "isready" => println!("readyok"),
        "position" => uci_position(&split_ws, &mut td.pos),
        "go" => uci_go(&split_ws, td, tt),
        "quit" => std::process::exit(0),
        // Non-UCI commands
        "display" | "d" | "print" | "show" => td.pos.display(),
        "perft" => {
            let depth: u32 = split_ws
                .get(1)
                .expect("Expected perft depth argument")
                .parse::<u32>()
                .expect("Error parsing perft depth");

            let start_time = Instant::now();
            let nodes: u64 = perft(&mut td.pos, depth);
            let nps: u64 = nodes * 1000 / (start_time.elapsed().as_millis().max(1) as u64);

            println!("{nodes} nodes {nps} nps");
        }
        "perftsplit" | "splitperft" | "perftdivide" | "divideperft" => {
            let depth: NonZeroU32 = split_ws
                .get(1)
                .unwrap_or_else(|| panic!("Expected {} depth argument", split_ws[0]))
                .parse::<NonZeroU32>()
                .unwrap_or_else(|_| panic!("Error parsing {} depth", split_ws[0]));

            perft_split(&mut td.pos, depth);
        }
        "bench" => {
            let depth: NonZeroU32 = split_ws
                .get(1)
                .map(|s| s.parse::<NonZeroU32>().expect("Error parsing bench depth"))
                .unwrap_or(DEFAULT_BENCH_DEPTH);

            bench(depth);
        }
        "eval" | "evaluate" | "evaluation" | "raweval" | "value" => {
            let mut both_accs = BothAccumulators::from(&td.pos);
            println!("eval {}", value_eval(&mut both_accs, td.pos.side_to_move()));
        }
        "policy" => {
            let mut both_accs = BothAccumulators::from(&td.pos);

            let mut policy: ScoredMoves =
                get_policy_logits::<false>(&mut both_accs, &td.pos, &td.pos.legal_moves(), None);

            softmax(&mut policy);

            policy.sort_by(|(_, a), (_, b)| b.partial_cmp(a).unwrap());

            for (mov, move_policy) in policy {
                println!("{mov}: {move_policy:.2}");
            }
        }
        "tt" | "TT" | "hash" | "Hash" | "hashfull" | "Hashfull" => {
            tt.print_fullness::<false>();
        }
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

            let src =
                Square::try_from(src_square_str).expect("Error parsing UCI move source square");

            let pt: PieceType = pos.at(src).expect("No piece in UCI move source square");

            let mov = ChessMove::from_uci(uci_move, pt).expect("Error parsing UCI move");
            pos.make_move(mov);
        }
    }
}

fn uci_go(tokens: &[&str], td: &mut ThreadData, tt: &mut TT) {
    let mut limits = SearchLimits::new(&Instant::now(), None, None, None, None);

    for window in tokens[1..].windows(2).step_by(2) {
        let (token1, token2): (&str, &str) = (window[0], window[1]);

        if token1 == "depth" {
            limits.max_depth = Some(token2.parse().expect("Error parsing depth"));
        } else if token1 == "nodes" {
            limits.max_nodes = Some(token2.parse().expect("Error parsing nodes"));
        } else if token1 == "movetime"
            || (token1 == "wtime" && td.pos.side_to_move() == Color::White)
            || (token1 == "btime" && td.pos.side_to_move() == Color::Black)
        {
            let mut time_ms: u64 = token2.parse::<i64>().expect("Error parsing time").max(0) as u64;
            time_ms = time_ms.saturating_sub(OVERHEAD_MS);

            if token1 != "movetime" {
                time_ms = (time_ms as f64 * HARD_TIME_PERCENTAGE).round() as u64;

                let soft_time_ms: u64 = (time_ms as f64 * SOFT_TIME_PERCENTAGE).round() as u64;
                limits.max_soft_duration = Some(Duration::from_millis(soft_time_ms));
            }

            limits.max_duration = Some(Duration::from_millis(time_ms));
        }
    }

    let best_move: Option<ChessMove> = search::<true>(&mut limits, td, tt).0;

    println!(
        "bestmove {}",
        best_move.map_or("0000".to_string(), |mov| mov.to_string())
    );
}
