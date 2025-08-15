#![allow(clippy::missing_transmute_annotations)]

mod chess;
mod nn;
mod search;
mod uci;

use search::{thread_data::ThreadData, tt::TT};
use std::num::NonZeroUsize;

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut td = ThreadData::new();

    let mut tt = TT::new(NonZeroUsize::new(32).unwrap());
    tt.print_size("info string");

    if args.len() > 1 {
        let input: String = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        uci::run_command(&input, &mut td, &mut tt);
        return;
    }

    let mut input = String::new();
    loop {
        std::io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");

        uci::run_command(&input, &mut td, &mut tt);
        input.clear();
    }
}
