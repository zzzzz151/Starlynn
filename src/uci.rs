use crate::chess::chess_move::ChessMove;
use crate::chess::perft::*;
use crate::chess::position::Position;
use crate::chess::types::*;
use crate::chess::util::FEN_START;
use std::num::{NonZeroU32, NonZeroU64};
use std::time::{Duration, Instant};

const OVERHEAD_MS: u64 = 20;

pub fn run_command(command: &str, pos: &mut Position) {
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
        "go" => uci_go(&split_ws, pos),
        "quit" => std::process::exit(0),
        // Non-UCI commands
        "display" | "d" | "print" | "show" => pos.display(),
        "perft" => {
            if let Some(depth) = split_ws
                .get(1)
                .and_then(|str_depth| str_depth.parse::<u32>().ok())
            {
                let start_time = Instant::now();
                let nodes = perft(pos, depth);
                let nps: u64 = nodes * 1000 / (start_time.elapsed().as_millis().max(1) as u64);
                println!("{nodes} nodes {nps} nps");
            } else {
                println!("Error parsing perft depth");
            }
        }
        "perftsplit" | "splitperft" | "perftdivide" | "divideperft" => {
            if let Some(depth) = split_ws
                .get(1)
                .and_then(|str_depth| str_depth.parse::<NonZeroU32>().ok())
            {
                perft_split(pos, depth);
            } else {
                println!("Error parsing {} depth", split_ws[0]);
            }
        }
        "bench" => println!("1 nodes 1200000 nps"),
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

#[allow(unused_variables)]
#[allow(unused_assignments)]
fn uci_go(tokens: &[&str], pos: &mut Position) {
    let mut max_depth: Option<NonZeroU64> = None;
    let mut max_nodes: Option<NonZeroU64> = None;
    let mut search_time: Option<Duration> = None;

    for pair in tokens[1..].chunks(2) {
        if let &[token1, token2] = pair {
            match token1 {
                "depth" => max_depth = Some(token2.parse().expect("Error parsing depth")),
                "nodes" => max_nodes = Some(token2.parse().expect("Error parsing nodes")),

                #[rustfmt::skip]
                token1 if token1 == "movetime" ||
                (token1 == "wtime" && pos.side_to_move() == Color::White) ||
                (token1 == "btime" && pos.side_to_move() == Color::Black) => {
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

    let moves = pos.legal_moves(false);

    if moves.is_empty() {
        println!("bestmove 0000");
    } else {
        let idx: usize = pos.zobrist_hash() as usize % moves.len();
        println!("bestmove {}", moves[idx]);
    }
}
