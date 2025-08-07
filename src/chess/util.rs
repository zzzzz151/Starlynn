use super::bitboard::Bitboard;
use super::types::PieceType;
use std::mem::transmute;

#[allow(dead_code)]
pub const FEN_START: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[allow(dead_code)]
pub const FEN_KIWIPETE: &str = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -";

#[allow(dead_code)]
pub const FEN_POS_3: &str = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";

#[allow(dead_code)]
pub const FEN_POS_4: &str = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";

#[allow(dead_code)]
pub const FEN_POS_5: &str = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";

#[allow(dead_code)]
pub const FEN_POS_6: &str =
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";

// [square1][square2]
pub static BETWEEN_EXCLUSIVE: [[Bitboard; 64]; 64] =
    unsafe { transmute(*include_bytes!("../embeds/between_exclusive.bin")) };

// [square1][square2]
pub static LINE_THRU: [[Bitboard; 64]; 64] =
    unsafe { transmute(*include_bytes!("../embeds/line_thru.bin")) };

pub const fn rand(mut state: u64) -> u64 {
    state ^= state << 13;
    state ^= state >> 7;
    state ^= state << 17;
    state
}

pub const ZOBRIST_COLOR: u64 = 207374551981581248;

// [color][piece_type][square]
pub const ZOBRIST_PIECES: [[[u64; 64]; 6]; 2] = {
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
    1301098977015881148,
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess::types::Square;
    use strum::IntoEnumIterator;

    #[test]
    fn test_between_exclusive() {
        for sq1 in Square::iter() {
            for sq2 in Square::iter() {
                assert_eq!(BETWEEN_EXCLUSIVE[sq1][sq2], BETWEEN_EXCLUSIVE[sq2][sq1]);

                if sq1 == sq2 {
                    assert!(BETWEEN_EXCLUSIVE[sq1][sq2].is_empty());
                }
            }
        }

        // Vertical

        assert!(BETWEEN_EXCLUSIVE[Square::B2][Square::B3].is_empty());
        assert!(BETWEEN_EXCLUSIVE[Square::B2][Square::A4].is_empty());

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::A2][Square::A4],
            Bitboard::from(Square::A3)
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::A2][Square::A8],
            Bitboard::from(282578800148480)
        );

        // Horizontal

        assert!(BETWEEN_EXCLUSIVE[Square::D4][Square::D5].is_empty());
        assert!(BETWEEN_EXCLUSIVE[Square::D4][Square::F5].is_empty());

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::D4][Square::D6],
            Bitboard::from(Square::D5)
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::D4][Square::D7],
            Bitboard::from([Square::D5, Square::D6].as_slice())
        );

        // Diagonal

        assert!(BETWEEN_EXCLUSIVE[Square::B1][Square::C2].is_empty());
        assert!(BETWEEN_EXCLUSIVE[Square::B2][Square::F7].is_empty());

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::B1][Square::D3],
            Bitboard::from(Square::C2)
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::C2][Square::G6],
            Bitboard::from(137707913216)
        );
    }

    #[test]
    fn test_line_thru() {
        for sq1 in Square::iter() {
            for sq2 in Square::iter() {
                assert_eq!(LINE_THRU[sq1][sq2], LINE_THRU[sq2][sq1]);

                if sq1 == sq2 {
                    assert!(LINE_THRU[sq1][sq2].is_empty());
                } else if sq1.file() == sq2.file() {
                    assert_eq!(LINE_THRU[sq1][sq2], Bitboard::from(sq1.file()));
                } else if sq1.rank() == sq2.rank() {
                    assert_eq!(LINE_THRU[sq1][sq2], Bitboard::from(sq1.rank()));
                }
            }
        }

        // No line
        assert!(LINE_THRU[Square::B2][Square::C4].is_empty());

        // Diagonal
        assert_eq!(
            LINE_THRU[Square::B2][Square::C3],
            Bitboard::from(9241421688590303745)
        );
    }
}
