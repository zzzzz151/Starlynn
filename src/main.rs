#![allow(clippy::missing_transmute_annotations)]

mod chess;
mod uci;
pub mod utils;

use chess::position::Position;
use chess::util::FEN_START;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut pos = Position::try_from(FEN_START).unwrap();

    if args.len() > 1 {
        let input: String = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        uci::run_command(&input, &mut pos);
        return;
    }

    let mut input = String::new();
    loop {
        std::io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");

        uci::run_command(&input, &mut pos);
        input.clear();
    }
}
