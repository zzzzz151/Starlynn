use std::env;
use std::io::{self};
use std::time::Instant;

mod types;
mod bitboard;
mod utils;
mod chess_move;
mod attacks;
mod pos_state;
mod movegen;
mod position;

use crate::pos_state::START_FEN;
use crate::position::Position;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = env::args().collect();
    let mut pos = Position::try_from(START_FEN).unwrap();

    if args.len() > 1 {
        let command = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        parse_command(command.as_str(), &mut pos);
        return;
    }

    // UCI loop
    loop {
        let mut command = String::new();
        io::stdin().read_line(&mut command).unwrap();

        if command.trim() == "quit" { break; }

        parse_command(command.as_str(), &mut pos);
    }
}

fn parse_command(command: &str, pos: &mut Position)
{
    let command = command.trim();
    let tokens: Vec<&str> = command.split_whitespace().map(|token| token.trim()).collect();

    if tokens.len() == 0 { return; }

    match tokens[0] {
        // UCI commands
        "uci" => {
            print!("id name Starlynn\n");
            print!("id author zzzzz\n");
            print!("option name Hash type spin default 32 min 1 max 65536\n");
            print!("option name Threads type spin default 1 min 1 max 256\n");
            println!("uciok");
        },
        "setoption" => { },
        "ucinewgame" => {
            *pos = Position::try_from(START_FEN).unwrap();
        },
        "isready" => {
            println!("readyok");
        },
        "position" => {
            if let Ok(new_pos) = uci_position(&tokens) {
                *pos = new_pos;
            }
            else {
                println!("info string Invalid position command");
            }
        },
        "go" => {
            uci_go(&tokens, pos);
        },
        // Non-UCI commands
        "d" | "display" | "print" | "show" => {
            pos.display();
        },
        "perft" => {
            if let Some(depth) = tokens.get(1).and_then(|str_depth| str_depth.parse::<u8>().ok())
            {
                let start_time = Instant::now();
                let leaves = pos.perft(depth);
                let nps = leaves as f64 / start_time.elapsed().as_secs_f64().max(0.001);
                println!("{} nodes {} nps", leaves, nps.round());
            }
            else {
                println!("Invalid perft command, expected \"perft <depth>\"");
            }
        },
        "perftsplit" | "splitperft" | "perftdivide" | "divideperft" =>
        {
            if let Some(depth) = tokens.get(1).and_then(|str_depth| str_depth.parse::<u8>().ok()) {
                pos.perft_split(depth);
            } else {
                println!("Invalid {0} command, expected {0} <depth>", tokens[0]);
            }
        },
        "bench" => {
            println!("1 nodes 1 nps");
        }
        _ => { }
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

fn uci_go(tokens: &Vec<&str>, pos: &mut Position)
{
    let mut _milliseconds = u64::MAX;

    if let Some(time_token_idx) = tokens.iter().position(
        |&token| token == pos.stm().to_string() + "time"
    ) {
        if let Some(str_milliseconds) = tokens.get(time_token_idx + 1) {
            if let Ok(i64_ms) = str_milliseconds.parse::<i64>() {
                _milliseconds = i64_ms.max(0) as u64;
            }
        }
    }

    _milliseconds = {
        let temp: f64 = (_milliseconds as f64 - 20.0).max(0.0) / 20.0;
        temp.round() as u64
    };

    let moves = pos.moves(true);

    if moves.len() == 0 {
        println!("bestmove 0000");
    }
    else {
        let mov = moves[pos.zobrist_hash() as usize % moves.len()];
        println!("bestmove {}", mov);
    }
}
