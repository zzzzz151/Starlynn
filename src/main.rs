#![allow(dead_code)]

use std::io::{self};

mod types;
mod bitboard;
mod utils;
mod chess_move;
mod attacks;
mod position;
mod movegen;

fn main() {
    println!("Starlynn by zzzzz");

    loop {
        let mut command = String::new();
        io::stdin().read_line(&mut command).unwrap();
        let command = command.trim();

        if command == "quit" {
            break;
        }

    }
}
