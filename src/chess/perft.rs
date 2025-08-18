use super::move_gen::MovesList;
use super::position::Position;
use std::num::NonZeroU32;
use std::time::Instant;

pub fn perft(pos: &mut Position, depth: u32) -> u64 {
    if depth == 0 {
        return 1;
    }

    let moves: MovesList = pos.legal_moves();

    if depth == 1 {
        return moves.len() as u64;
    }

    let mut nodes: u64 = 0;

    for mov in moves {
        pos.make_move(mov);
        nodes += perft(pos, depth - 1);
        pos.undo_move();
    }

    nodes
}

pub fn perft_split(pos: &mut Position, depth: NonZeroU32) {
    let start_time = Instant::now();
    let mut total_nodes: u64 = 0;

    for mov in pos.legal_moves() {
        pos.make_move(mov);

        let nodes: u64 = perft(pos, depth.get() - 1);
        println!("{mov}: {nodes}");
        total_nodes += nodes;

        pos.undo_move();
    }

    let nps: u64 = total_nodes * 1000 / (start_time.elapsed().as_millis().max(1) as u64);
    println!("{total_nodes} nodes {nps} nps");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess::util::{FEN_KIWIPETE, FEN_POS_3, FEN_POS_4, FEN_POS_5, FEN_POS_6, FEN_START};

    #[test]
    pub fn test_perft() {
        let mut pos = Position::try_from(FEN_START).unwrap();

        assert_eq!(perft(&mut pos, 0), 1);
        assert_eq!(perft(&mut pos, 1), 20);
        assert_eq!(perft(&mut pos, 2), 400);
        assert_eq!(perft(&mut pos, 3), 8902);
        assert_eq!(perft(&mut pos, 4), 197_281);
        assert_eq!(perft(&mut pos, 5), 4_865_609);
        assert_eq!(perft(&mut pos, 6), 119_060_324);

        pos = Position::try_from(FEN_KIWIPETE).unwrap();

        assert_eq!(perft(&mut pos, 1), 48);
        assert_eq!(perft(&mut pos, 2), 2039);
        assert_eq!(perft(&mut pos, 3), 97_862);
        assert_eq!(perft(&mut pos, 4), 4_085_603);
        assert_eq!(perft(&mut pos, 5), 193_690_690);

        pos = Position::try_from(FEN_POS_3).unwrap();

        assert_eq!(perft(&mut pos, 1), 14);
        assert_eq!(perft(&mut pos, 2), 191);
        assert_eq!(perft(&mut pos, 3), 2812);
        assert_eq!(perft(&mut pos, 4), 43_238);
        assert_eq!(perft(&mut pos, 5), 674_624);
        assert_eq!(perft(&mut pos, 6), 11_030_083);

        pos = Position::try_from(FEN_POS_4).unwrap();

        assert_eq!(perft(&mut pos, 1), 6);
        assert_eq!(perft(&mut pos, 2), 264);
        assert_eq!(perft(&mut pos, 3), 9467);
        assert_eq!(perft(&mut pos, 4), 422_333);
        assert_eq!(perft(&mut pos, 5), 15_833_292);

        pos = Position::try_from(FEN_POS_5).unwrap();

        assert_eq!(perft(&mut pos, 1), 44);
        assert_eq!(perft(&mut pos, 2), 1486);
        assert_eq!(perft(&mut pos, 3), 62_379);
        assert_eq!(perft(&mut pos, 4), 2_103_487);
        assert_eq!(perft(&mut pos, 5), 89_941_194);

        pos = Position::try_from(FEN_POS_6).unwrap();

        assert_eq!(perft(&mut pos, 1), 46);
        assert_eq!(perft(&mut pos, 2), 2079);
        assert_eq!(perft(&mut pos, 3), 89_890);
        assert_eq!(perft(&mut pos, 4), 3_894_594);
    }
}
