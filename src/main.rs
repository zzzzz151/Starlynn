#![allow(clippy::missing_transmute_annotations)]

mod chess;
mod nn;
mod search;
mod uci;

use search::search::ThreadData;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut td = ThreadData::new();

    if args.len() > 1 {
        let input: String = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        uci::run_command(&input, &mut td);
        return;
    }

    let mut input = String::new();
    loop {
        std::io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");

        uci::run_command(&input, &mut td);
        input.clear();
    }
}
