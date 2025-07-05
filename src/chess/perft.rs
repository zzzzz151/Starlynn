use super::pos_state::PosState;

pub fn perft(pos_state: &PosState, depth: i32) -> u64
{
    assert!(depth >= 0, "Perft depth must be greater or equal to 0");

    if depth == 0 { return 1; }

    let moves = pos_state.legal_moves(true);

    if depth == 1 { return moves.len() as u64; }

    let mut nodes: u64 = 0;

    for mov in moves
    {
        let mut new_pos_state = pos_state.clone();
        new_pos_state.make_move(mov);
        nodes += perft(&new_pos_state, depth - 1);
    }

    nodes
}

pub fn perft_split(pos_state: &PosState, depth: i32)
{
    assert!(depth > 0, "Perft split depth must be greater than 0");

    let start_time = std::time::Instant::now();
    let mut total_nodes: u64 = 0;

    for mov in pos_state.legal_moves(true)
    {
        let mut new_pos_state = pos_state.clone();
        new_pos_state.make_move(mov);

        let nodes = perft(&new_pos_state, depth - 1);
        println!("{}: {}", mov, nodes);
        total_nodes += nodes;
    }

    let nps: u64 = (total_nodes as f64 / start_time.elapsed().as_secs_f64()) as u64;
    println!("{} nodes {} nps", total_nodes, nps);
}

#[cfg(test)]
mod tests
{
    use super::*;
    use crate::chess::util::{FEN_START, FEN_KIWIPETE, FEN_POS_3, FEN_POS_4, FEN_POS_5, FEN_POS_6};

    #[test]
    pub fn test_perft()
    {
        let mut pos_state = PosState::try_from(FEN_START).unwrap();

        assert!(perft(&pos_state, 0) == 1);
        assert!(perft(&pos_state, 1) == 20);
        assert!(perft(&pos_state, 2) == 400);
        assert!(perft(&pos_state, 3) == 8902);
        assert!(perft(&pos_state, 4) == 197_281);
        assert!(perft(&pos_state, 5) == 4_865_609);
        assert!(perft(&pos_state, 6) == 119_060_324);

        pos_state = PosState::try_from(FEN_KIWIPETE).unwrap();

        assert!(perft(&pos_state, 1) == 48);
        assert!(perft(&pos_state, 2) == 2039);
        assert!(perft(&pos_state, 3) == 97_862);
        assert!(perft(&pos_state, 4) == 4_085_603);
        assert!(perft(&pos_state, 5) == 193_690_690);

        pos_state = PosState::try_from(FEN_POS_3).unwrap();

        assert!(perft(&pos_state, 1) == 14);
        assert!(perft(&pos_state, 2) == 191);
        assert!(perft(&pos_state, 3) == 2812);
        assert!(perft(&pos_state, 4) == 43_238);
        assert!(perft(&pos_state, 5) == 674_624);
        assert!(perft(&pos_state, 6) == 11_030_083);

        pos_state = PosState::try_from(FEN_POS_4).unwrap();

        assert!(perft(&pos_state, 1) == 6);
        assert!(perft(&pos_state, 2) == 264);
        assert!(perft(&pos_state, 3) == 9467);
        assert!(perft(&pos_state, 4) == 422_333);
        assert!(perft(&pos_state, 5) == 15_833_292);

        pos_state = PosState::try_from(FEN_POS_5).unwrap();

        assert!(perft(&pos_state, 1) == 44);
        assert!(perft(&pos_state, 2) == 1486);
        assert!(perft(&pos_state, 3) == 62_379);
        assert!(perft(&pos_state, 4) == 2_103_487);
        assert!(perft(&pos_state, 5) == 89_941_194);

        pos_state = PosState::try_from(FEN_POS_6).unwrap();

        assert!(perft(&pos_state, 1) == 46);
        assert!(perft(&pos_state, 2) == 2079);
        assert!(perft(&pos_state, 3) == 89_890);
        assert!(perft(&pos_state, 4) == 3_894_594);
    }

}
