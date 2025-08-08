#![allow(clippy::missing_transmute_annotations)]

mod chess;
mod search;
mod uci;

use chess::{position::Position, util::FEN_START};
use search::searcher::Searcher;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut pos = Position::try_from(FEN_START).unwrap();
    let mut searcher = Searcher::new();

    if args.len() > 1 {
        let input: String = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        uci::run_command(&input, &mut pos, &mut searcher);
        return;
    }

    let mut input = String::new();
    loop {
        std::io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");

        uci::run_command(&input, &mut pos, &mut searcher);
        input.clear();
    }
}
