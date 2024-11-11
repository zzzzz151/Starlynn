use crate::types::Square;
use crate::bitboard::Bitboard;

pub fn bishop_attacks(square: Square, occupancy: Bitboard) -> Bitboard
{
    let blockers = occupancy & BISHOP_ATKS_EMPTY_BOARD_EXCLUDING_LAST_SQ_EACH_DIR[square];
    let idx = (u64::from(blockers).wrapping_mul(BISHOP_MAGICS[square])) >> BISHOP_SHIFTS[square];
    BISHOP_ATTACKS_TABLE[square][idx as usize]
}

pub fn rook_attacks(square: Square, occupancy: Bitboard) -> Bitboard
{
    let blockers = occupancy & ROOK_ATKS_EMPTY_BOARD_EXCLUDING_LAST_SQ_EACH_DIR[square];
    let idx = (u64::from(blockers).wrapping_mul(ROOK_MAGICS[square])) >> ROOK_SHIFTS[square];
    ROOK_ATTACKS_TABLE[square][idx as usize]
}

pub fn queen_attacks(square: Square, occupancy: Bitboard) -> Bitboard
{
    bishop_attacks(square, occupancy) | rook_attacks(square, occupancy)
}

pub const PAWN_ATTACKS: [[Bitboard; 64]; 2] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/pawn_attacks.bin"))
};

pub const KNIGHT_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/knight_attacks.bin"))
};

pub const KING_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/king_attacks.bin"))
};

const BISHOP_ATKS_EMPTY_BOARD_EXCLUDING_LAST_SQ_EACH_DIR: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(
        "embeds/bishop_atks_empty_board_excluding_last_sq_each_dir.bin"
    ))
};

const ROOK_ATKS_EMPTY_BOARD_EXCLUDING_LAST_SQ_EACH_DIR: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(
        "embeds/rook_atks_empty_board_excluding_last_sq_each_dir.bin"
    ))
};

const BISHOP_ATTACKS_TABLE: [[Bitboard; 1usize << 9]; 64] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/bishop_attacks.bin"))
};

const ROOK_ATTACKS_TABLE: [[Bitboard; 1usize << 12]; 64] = unsafe {
    std::mem::transmute(*include_bytes!("embeds/rook_attacks.bin"))
};

const BISHOP_SHIFTS: [u64; 64] = [
    58, 59, 59, 59, 59, 59, 59, 58,
    59, 59, 59, 59, 59, 59, 59, 59,
    59, 59, 57, 57, 57, 57, 59, 59,
    59, 59, 57, 55, 55, 57, 59, 59,
    59, 59, 57, 55, 55, 57, 59, 59,
    59, 59, 57, 57, 57, 57, 59, 59,
    59, 59, 59, 59, 59, 59, 59, 59,
    58, 59, 59, 59, 59, 59, 59, 58,
];

const ROOK_SHIFTS: [u64; 64] = [
    52, 53, 53, 53, 53, 53, 53, 52,
    53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53,
    52, 53, 53, 53, 53, 53, 53, 52,
];

const BISHOP_MAGICS: [u64; 64] = [
    0x89a1121896040240, 0x2004844802002010, 0x2068080051921000, 0x62880a0220200808,
    0x4042004000000, 0x100822020200011, 0xc00444222012000a, 0x28808801216001,
    0x400492088408100, 0x201c401040c0084, 0x840800910a0010, 0x82080240060,
    0x2000840504006000, 0x30010c4108405004, 0x1008005410080802, 0x8144042209100900,
    0x208081020014400, 0x4800201208ca00, 0xf18140408012008, 0x1004002802102001,
    0x841000820080811, 0x40200200a42008, 0x800054042000, 0x88010400410c9000,
    0x520040470104290, 0x1004040051500081, 0x2002081833080021, 0x400c00c010142,
    0x941408200c002000, 0x658810000806011, 0x188071040440a00, 0x4800404002011c00,
    0x104442040404200, 0x511080202091021, 0x4022401120400, 0x80c0040400080120,
    0x8040010040820802, 0x480810700020090, 0x102008e00040242, 0x809005202050100,
    0x8002024220104080, 0x431008804142000, 0x19001802081400, 0x200014208040080,
    0x3308082008200100, 0x41010500040c020, 0x4012020c04210308, 0x208220a202004080,
    0x111040120082000, 0x6803040141280a00, 0x2101004202410000, 0x8200000041108022,
    0x21082088000, 0x2410204010040, 0x40100400809000, 0x822088220820214,
    0x40808090012004, 0x910224040218c9, 0x402814422015008, 0x90014004842410,
    0x1000042304105, 0x10008830412a00, 0x2520081090008908, 0x40102000a0a60140,
];

const ROOK_MAGICS: [u64; 64] = [
    0xa8002c000108020, 0x6c00049b0002001, 0x100200010090040, 0x2480041000800801,
    0x280028004000800, 0x900410008040022, 0x280020001001080, 0x2880002041000080,
    0xa000800080400034, 0x4808020004000, 0x2290802004801000, 0x411000d00100020,
    0x402800800040080, 0xb000401004208, 0x2409000100040200, 0x1002100004082,
    0x22878001e24000, 0x1090810021004010, 0x801030040200012, 0x500808008001000,
    0xa08018014000880, 0x8000808004000200, 0x201008080010200, 0x801020000441091,
    0x800080204005, 0x1040200040100048, 0x120200402082, 0xd14880480100080,
    0x12040280080080, 0x100040080020080, 0x9020010080800200, 0x813241200148449,
    0x491604001800080, 0x100401000402001, 0x4820010021001040, 0x400402202000812,
    0x209009005000802, 0x810800601800400, 0x4301083214000150, 0x204026458e001401,
    0x40204000808000, 0x8001008040010020, 0x8410820820420010, 0x1003001000090020,
    0x804040008008080, 0x12000810020004, 0x1000100200040208, 0x430000a044020001,
    0x280009023410300, 0xe0100040002240, 0x200100401700, 0x2244100408008080,
    0x8000400801980, 0x2000810040200, 0x8010100228810400, 0x2000009044210200,
    0x4080008040102101, 0x40002080411d01, 0x2005524060000901, 0x502001008400422,
    0x489a000810200402, 0x1004400080a13, 0x4000011008020084, 0x26002114058042,
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Color;

    #[test]
    fn test_non_sliders_attacks()
    {
        assert_eq!(
            PAWN_ATTACKS[Color::White][Square::E4],
            Bitboard::from(Square::D5) | Bitboard::from(Square::F5)
        );

        assert_eq!(PAWN_ATTACKS[Color::Black][Square::A2], Bitboard::from(Square::B1));

        assert_eq!(KNIGHT_ATTACKS[Square::A3], Bitboard::from(8657044482u64));
        assert_eq!(KNIGHT_ATTACKS[Square::D4], Bitboard::from(22136263676928u64));

        assert_eq!(KING_ATTACKS[Square::B1], Bitboard::from(1797u64));
        assert_eq!(KING_ATTACKS[Square::D4], Bitboard::from(120596463616u64));
    }

    #[test]
    fn test_sliders_attacks()
    {
        assert_eq!(
            bishop_attacks(Square::B2, Bitboard::EMPTY),
            Bitboard::from(9241421688590368773u64)
        );

        assert_eq!(
            bishop_attacks(Square::B2, Bitboard::from(68719542784u64)),
            Bitboard::from(68854022149u64)
        );

        assert_eq!(
            rook_attacks(Square::B1, Bitboard::EMPTY),
            Bitboard::from(144680345676153597u64)
        );

        assert_eq!(
            rook_attacks(Square::B1, Bitboard::from(562949953421442u64)),
            Bitboard::from(565157600297725u64)
        );
    }
}
