use crate::types::{Square, PieceType};

pub const fn castling_rook_from_to(king_to: Square) -> (Square, Square)
{
    match king_to {
        Square::C1 => (Square::A1, Square::D1),
        Square::G1 => (Square::H1, Square::F1),
        Square::C8 => (Square::A8, Square::D8),
        Square::G8 => (Square::H8, Square::F8),
        _ => panic!("Invalid castling king target square")
    }
}

pub const fn rand(mut state: u64) -> u64
{
    state ^= state << 13;
    state ^= state >> 7;
    state ^= state << 17;
    state
}

pub const ZOBRIST_COLOR: u64 = 207374551981581248;

// [color][piece_type][square]
pub const ZOBRIST_PIECES: [[[u64; 64]; 6]; 2] =
{
    let mut array = [[[0; 64]; 6]; 2];
    let mut rng_state: u64 = 180_620_142;

    let mut color: usize = 0;
    while color < 2 {
        let mut piece_type: usize = 0;

        while piece_type <= (PieceType::King as usize) {
            let mut square: usize = 0;

            while square < 64 {
                rng_state = rand(rng_state);
                array[color][piece_type][square] = rng_state;
                square += 1;
            }

            piece_type += 1;
        }

        color += 1;
    }

    array
};

// [file]
pub const ZOBRIST_FILES: [u64; 8] = [
    1828318524018422091,
    8752110151114172124,
    13824397244246208200,
    12617244103162170911,
    16829371578114820122,
    1209965196757994146,
    11318421891342381489,
    1301098977015881148
];
