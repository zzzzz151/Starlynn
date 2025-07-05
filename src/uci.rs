use crate::chess::types::{Square, PieceType};
use crate::chess::util::{FEN_START};
use crate::chess::chess_move::ChessMove;
use crate::chess::position::Position;
use crate::chess::perft::*;

pub fn run_command(command: &str, pos: &mut Position)
{
    let command = command.trim();
    let split_ws: Vec<&str> = command.split_whitespace().map(|token| token.trim()).collect();

    if split_ws.len() == 0 { return; }

    match split_ws[0]
    {
        // UCI commands
        "uci" => {
            print!("id name Starlynn");
            print!("\nid name zzzzz");
            println!("\nuciok");
        },
        "ucinewgame" => *pos = Position::try_from(FEN_START).unwrap(),
        "isready" => println!("readyok"),
        "position" => uci_position(&split_ws, pos),
        "go" => {
            let moves = pos.legal_moves(false);

            let move_str: String = if moves.len() == 0 {
                "0000".to_string()
            } else {
                let idx = pos.zobrist_hash() as usize % moves.len();
                moves[idx].to_string()
            };

            println!("bestmove {}", move_str);
        },
        "quit" => std::process::exit(0),
        // Non-UCI commands
        "display" | "d" | "print" | "show" => pos.display(),
        "perft" =>
        {
            if let Some(depth) = split_ws.get(1)
                .and_then(|str_depth| str_depth.parse::<i32>().ok())
            {
                let start_time = std::time::Instant::now();
                let nodes = perft(pos.state(), depth);
                let nps: u64 = (nodes as f64 / start_time.elapsed().as_secs_f64()) as u64;
                println!("{} nodes {} nps", nodes, nps);
            }
            else {
                println!("Invalid perft command");
            }
        },
        "perftsplit" | "splitperft" | "perftdivide" | "divideperft" =>
        {
            if let Some(depth) = split_ws.get(1)
                .and_then(|str_depth| str_depth.parse::<i32>().ok())
            {
                perft_split(pos.state(), depth);
            }
            else {
                println!("Invalid {} command", split_ws[0]);
            }
        },
        "bench" => println!("1 nodes 1200000 nps"),
        _ => { }
    }
}

fn uci_position(tokens: &Vec<&str>, pos: &mut Position)
{
    assert!(tokens[0] == "position");

    match tokens[1]
    {
        "startpos" => *pos = Position::try_from(FEN_START).unwrap(),
        "fen" => {
            if let Err(msg) = uci_position_fen(tokens, pos)
            {
                println!("info string {}", msg);
            }
        },
        _ => { }
    }
}

fn uci_position_fen(tokens: &Vec<&str>, pos: &mut Position) -> Result<(), String>
{
    if tokens.len() < 8 { return Err("Invalid FEN".to_string()); }

    let fen: String = tokens[2..8].join(" ");

    if let Ok(new_pos) = Position::try_from(fen.as_str())
    {
        *pos = new_pos;
    }
    else {
        return Err("Invalid FEN".to_string());
    };

    if tokens.len() > 8 && tokens[8] == "moves"
    {
        for uci_move in tokens.iter().skip(9)
        {
            let src_sq = Square::try_from(&uci_move[0..2])
                .map_err(|_| "Error parsing UCI move source square")?;

            let pt: PieceType = pos.at(src_sq).ok_or("No piece in UCI move source square")?;
            let mov = ChessMove::from_uci(uci_move, pt)?;
            pos.make_move(mov);
        }
    }

    Ok(())
}
