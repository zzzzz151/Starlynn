use crate::nn::{accumulator::BothAccumulators, moves_map::map_moves_1880, value_policy_heads::*};
use std::fs::File;
use std::io::Write;
use std::mem::size_of;
use std::num::NonZeroU32;
use std::slice::from_raw_parts;
use std::time::{Duration, Instant};

use crate::chess::{
    chess_move::ChessMove,
    perft::{perft, perft_split},
    position::Position,
    types::{Color, PieceType, Square},
    util::FEN_START,
};

use crate::search::{
    bench::{DEFAULT_BENCH_DEPTH, bench},
    search::*,
};

const OVERHEAD_MS: u64 = 20;

pub fn run_command(command: &str, td: &mut ThreadData) {
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
        "ucinewgame" => td.pos = Position::try_from(FEN_START).unwrap(),
        "isready" => println!("readyok"),
        "position" => uci_position(&split_ws, &mut td.pos),
        "go" => uci_go(&split_ws, td),
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

            let mut policy =
                get_policy_logits(&mut both_accs, td.pos.side_to_move(), &td.pos.legal_moves());

            softmax(&mut policy);

            while let Some((mov, move_policy)) = remove_best_move(&mut policy) {
                println!("{}: {:.2}", mov, move_policy);
            }
        }
        "mapmoves1880" | "map_moves_1880" | "movesmap1880" | "moves_map_1880" => {
            let out_file_name: &str = split_ws.get(1).unwrap_or(&"moves_map_1880.bin");

            let mut out_file: File = File::create(out_file_name)
                .unwrap_or_else(|_| panic!("Failed to create file {out_file_name}"));

            let moves_map_1880: [[[i16; 7]; 64]; 64] = map_moves_1880();

            let byte_slice: &[u8] = unsafe {
                from_raw_parts(
                    moves_map_1880.as_ptr() as *const u8,
                    size_of::<[[[i16; 7]; 64]; 64]>(),
                )
            };

            out_file
                .write_all(byte_slice)
                .unwrap_or_else(|_| panic!("Failed to write to file {out_file_name}"));

            println!("Sucessfully wrote {out_file_name}");
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

fn uci_go(tokens: &[&str], td: &mut ThreadData) {
    let mut limits = SearchLimits {
        start_time: Instant::now(),
        max_depth: None,
        max_nodes: None,
        max_duration: None,
        max_duration_hit: false,
    };

    for pair in tokens[1..].chunks(2) {
        if let &[token1, token2] = pair {
            match token1 {
                "depth" => limits.max_depth = Some(token2.parse().expect("Error parsing depth")),
                "nodes" => limits.max_nodes = Some(token2.parse().expect("Error parsing nodes")),

                token1
                    if token1 == "movetime"
                        || (token1 == "wtime" && td.pos.side_to_move() == Color::White)
                        || (token1 == "btime" && td.pos.side_to_move() == Color::Black) =>
                {
                    let mut time_ms: u64 =
                        token2.parse::<i64>().expect("Error parsing time").max(0) as u64;

                    time_ms = time_ms.saturating_sub(OVERHEAD_MS);

                    if token1 != "movetime" {
                        time_ms /= 25;
                    }

                    limits.max_duration = Some(Duration::from_millis(time_ms));
                }
                _ => {}
            }
        };
    }

    let best_move: Option<ChessMove> = search(&mut limits, td, true).0;

    println!(
        "bestmove {}",
        best_move.map_or("0000".to_string(), |mov| mov.to_string())
    );
}
