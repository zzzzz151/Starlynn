use std::io::{self};
use std::time::{Instant, Duration};

mod types;
mod bitboard;
mod utils;
mod chess_move;
mod attacks;
mod pos_state;
mod movegen;
mod position;
mod eval;
mod node;
mod search;
mod bench;

use crate::pos_state::START_FEN;
use crate::position::Position;
use crate::search::Tree;
use crate::bench::bench;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut pos = Position::try_from(START_FEN).unwrap();
    let mut tree = Tree::new(32);

    // If a command was passed in program args, run it and exit
    if args.len() > 1 {
        let command = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        run_command(command.as_str(), &mut pos, &mut tree);
        return;
    }

    // UCI loop
    loop {
        let mut command = String::new();
        io::stdin().read_line(&mut command).unwrap();

        if command.trim() == "quit" { break; }

        run_command(command.as_str(), &mut pos, &mut tree);
    }
}

fn run_command(command: &str, pos: &mut Position, tree: &mut Tree)
{
    let command = command.trim();

    let tokens: Vec<&str> = command
        .split_whitespace().map(|token| token.trim()).collect();

    if tokens.len() == 0 { return; }

    match tokens[0] {
        // UCI commands
        "uci" => {
            print!("id name Starlynn\n");
            print!("id author zzzzz\n");
            print!("option name Hash type spin default 32 min 1 max 32768\n");
            print!("option name Threads type spin default 1 min 1 max 256\n");
            println!("uciok");
        },
        "setoption" => uci_setoption(&tokens, tree),
        "ucinewgame" => *pos = Position::try_from(START_FEN).unwrap(),
        "isready" => println!("readyok"),
        "position" => {
            if let Ok(new_pos) = uci_position(&tokens) {
                *pos = new_pos;
            }
            else {
                println!("info string Invalid position command");
            }
        },
        "go" => uci_go(&tokens, pos, tree),
        // Non-UCI commands
        "d" | "display" | "print" | "show" => pos.display(),
        "perft" => {
            if let Some(depth) = tokens.get(1)
                .and_then(|str_depth| str_depth.parse::<u8>().ok())
            {
                let start_time = Instant::now();
                let leaves = pos.perft(depth);

                let nps = leaves * 1000
                        / (start_time.elapsed().as_micros().max(1) as u64);

                println!("{} nodes {} nps", leaves, nps);
            }
            else {
                println!("Invalid perft command, expected \"perft <depth>\"");
            }
        },
        "perftsplit" | "splitperft" | "perftdivide" | "divideperft" =>
        {
            if let Some(depth) = tokens.get(1)
                .and_then(|str_depth| str_depth.parse::<u8>().ok())
            {
                pos.perft_split(depth);
            } else {
                println!("Invalid {0} command, expected {0} <depth>", tokens[0]);
            }
        },
        "tree" => {
            println!("{}", tree);
        }
        "bench" => {
            let depth = tokens.get(1)
                .and_then(|token| token.parse::<i64>().ok()).unwrap_or(4);

            bench(depth.max(1) as u8);
        },
        _ => { }
    }
}

fn uci_setoption(tokens: &Vec<&str>, tree: &mut Tree)
{
    if let (Some(name), Some(str_value)) = (tokens.get(2), tokens.get(4))
    {
        if name.to_lowercase() == "hash" {
            let mib = str_value.parse::<i64>().unwrap_or_else(|_| 1);
            *tree = Tree::new(mib.clamp(1, 32768) as usize);
        }
    }
    else {
        println!("info string Invalid setoption command");
    }
}

#[derive(Debug)]
pub struct InvalidUciPosition;

fn uci_position(tokens: &Vec<&str>) -> Result<Position, InvalidUciPosition>
{
    let moves_token_idx: Option<usize> = tokens.iter().position(|&token| token == "moves");

    let mut pos: Position = if tokens[1] == "startpos" {
        Position::try_from(START_FEN).unwrap()
    }
    else if tokens[1] == "fen" {
        let end = moves_token_idx.unwrap_or(tokens.len());
        let fen = tokens[2..end].join(" ");
        Position::try_from(fen.as_str()).map_err(|_| InvalidUciPosition)?
    }
    else {
        return Err(InvalidUciPosition);
    };

    if let Some(moves_token_idx) = moves_token_idx {
        for uci_move in &tokens[(moves_token_idx + 1)..] {
            let mov = pos.uci_to_move(uci_move).map_err(|_| InvalidUciPosition)?;
            pos.make_move(mov);
        }
    }

    Ok(pos)
}

fn uci_go(tokens: &Vec<&str>, pos: &mut Position, tree: &mut Tree)
{
    let start_time = Instant::now();
    let mut milliseconds = u64::MAX;
    let mut moves_to_go = 20;
    let mut is_move_time = false;

    let mut depth = u8::MAX;
    let mut nodes = u64::MAX;

    let stm_time_token = pos.stm().to_string() + "time";

    for pair in tokens.iter().skip(1).collect::<Vec<_>>().chunks(2)
    {
        if pair.len() == 1 { break; }

        let token = *(pair[0]);

        let value = pair[1].parse::<i64>()
            .expect("Invalid go command").max(0) as u64;

        match token {
            _ if token == stm_time_token => milliseconds = value,
            "movestogo" => moves_to_go = value.max(1),
            "movetime" => {
                is_move_time = true;
                milliseconds = value;
            },
            "depth" => depth = value.clamp(0, u8::MAX as u64) as u8,
            "nodes" => nodes = value.max(1),
            _ => {},
        }
    }

    if milliseconds != u64::MAX {
        milliseconds = (milliseconds as i64 - 20).max(0) as u64;

        if !is_move_time {
            let f64_ms = milliseconds as f64 / (moves_to_go as f64);
            milliseconds = f64_ms.round() as u64;
        }
    }

    println!("info string Starting {milliseconds} milliseconds search");

    if let Some(mov) = tree.search(
        pos, &start_time, &Duration::from_millis(milliseconds), depth, nodes, true
    ).0 {
        println!("bestmove {mov}");
    }
    else {
        println!("bestmove 0000");
    }
}
