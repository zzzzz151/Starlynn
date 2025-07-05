mod chess;
mod uci;

use chess::util::{FEN_START};
use chess::position::Position;
use uci::run_command;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut pos = Position::try_from(FEN_START).unwrap();

    if args.len() > 1
    {
        let mut input: String = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        run_command(&mut input, &mut pos);
        return;
    }

    let mut input = String::new();
    loop {
        std::io::stdin().read_line(&mut input).unwrap();
        run_command(&mut input, &mut pos);
        input.clear();
    }

}
