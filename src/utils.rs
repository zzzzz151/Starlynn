use crate::types::PieceType;
use crate::bitboard::Bitboard;

pub const BETWEEN_EXCLUSIVE: [[Bitboard; 64]; 64] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/between_exclusive.bin"))
};

pub const LINE_THRU: [[Bitboard; 64]; 64] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/line_thru.bin"))
};

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

#[cfg(test)]
mod tests {
    use super::*;
    use strum::IntoEnumIterator;
    use crate::types::Square;

    #[test]
    fn test_between_exclusive() {
        // Vertical

        assert_eq!(BETWEEN_EXCLUSIVE[Square::B2][Square::B3], Bitboard::EMPTY);

        assert_eq!(BETWEEN_EXCLUSIVE[Square::B2][Square::A4], Bitboard::EMPTY);

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::A2][Square::A4],
            Bitboard::from(Square::A3)
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::A2][Square::A8],
            Bitboard::from(282578800148480u64)
        );

        // Horizontal

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::D4][Square::D5], Bitboard::EMPTY
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::D4][Square::F5], Bitboard::EMPTY
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::D4][Square::D6],
            Bitboard::from(Square::D5)
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::D4][Square::D7],
            Bitboard::from(Square::D5) | Bitboard::from(Square::D6)
        );

        // Diagonal

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::B1][Square::C2], Bitboard::EMPTY
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::B2][Square::F7], Bitboard::EMPTY
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::B1][Square::D3],
            Bitboard::from(Square::C2)
        );

        assert_eq!(
            BETWEEN_EXCLUSIVE[Square::C2][Square::G6],
            Bitboard::from(137707913216u64)
        );

        for sq1 in Square::iter() {
            for sq2 in Square::iter() {
                assert_eq!(
                    BETWEEN_EXCLUSIVE[sq1][sq2], BETWEEN_EXCLUSIVE[sq2][sq1]
                );

                if sq1 == sq2 {
                    assert_eq!(BETWEEN_EXCLUSIVE[sq1][sq2], Bitboard::EMPTY);
                }
            }
        }
    }

    #[test]
    fn test_line_thru() {
        // No line
        assert_eq!(LINE_THRU[Square::B2][Square::C4], Bitboard::EMPTY);

        // Diagonal
        assert_eq!(
            LINE_THRU[Square::B2][Square::C3],
            Bitboard::from(9241421688590303745u64)
        );

        for sq1 in Square::iter() {
            for sq2 in Square::iter() {
                assert_eq!(
                    LINE_THRU[sq1][sq2], LINE_THRU[sq2][sq1]
                );

                if sq1 == sq2 {
                    assert_eq!(LINE_THRU[sq1][sq2], Bitboard::EMPTY);
                }
                else if sq1.file() == sq2.file()
                {
                    assert_eq!(
                        LINE_THRU[sq1][sq2],
                        Bitboard::FILE[sq1.file()]
                    );
                }
                else if sq1.rank() == sq2.rank()
                {
                    assert_eq!(
                        LINE_THRU[sq1][sq2],
                        Bitboard::RANK[sq1.rank()]
                    );
                }
            }
        }
    }
}
