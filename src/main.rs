#![allow(dead_code)]

use std::io::{self};

mod types;
mod bitboard;
mod utils;
mod chess_move;
mod attacks;
mod position;
mod movegen;

use crate::position::{Position, START_FEN};

fn main() {
    println!("Starlynn by zzzzz");

    let mut pos = Position::from(START_FEN);
    pos.display();

    println!("");
    pos = Position::from("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -");
    pos.display();

    loop {
        let mut command = String::new();
        io::stdin().read_line(&mut command).unwrap();
        let command = command.trim();

        if command == "quit" {
            break;
        }

    }
}
