use super::attacks::*;
use super::bitboard::Bitboard;
use super::chess_move::ChessMove;
use super::types::*;
use super::util::*;
use std::cmp::PartialEq;
use std::mem::transmute;
use std::num::NonZeroU16;
use strum::IntoEnumIterator;

// #[repr(packed)]
#[derive(Clone, Debug)]
pub struct PosState {
    stm: Color,
    color_bbs: [Bitboard; 2], // [color]
    piece_bbs: [Bitboard; 6], // [piece_type]
    castling_rights: Bitboard,
    en_passant_square: Option<Square>,
    plies_since_pawn_or_capture: u16,
    move_counter: NonZeroU16,
    last_move: Option<ChessMove>,
    pt_captured: Option<PieceType>,
    checkers: Bitboard,
    zobrist_hash: u64,
}

impl Eq for PosState {}

impl PartialEq for PosState {
    #[rustfmt::skip]
    fn eq(&self, other: &Self) -> bool {
        self.stm == other.stm &&
        self.color_bbs == other.color_bbs &&
        self.piece_bbs == other.piece_bbs &&
        self.castling_rights == other.castling_rights &&
        self.en_passant_square == other.en_passant_square &&
        self.plies_since_pawn_or_capture == other.plies_since_pawn_or_capture &&
        self.move_counter == other.move_counter &&
        self.checkers == other.checkers &&
        self.zobrist_hash == other.zobrist_hash
    }
}

impl TryFrom<&str> for PosState {
    type Error = String;

    fn try_from(fen: &str) -> Result<Self, Self::Error> {
        let split_ws: Vec<&str> = fen.split_whitespace().map(|s| s.trim()).collect();

        if split_ws.len() < 4 || split_ws.len() > 6 {
            return Err("FEN must have exactly 4, 5 or 6 tokens separated by space(s)".to_string());
        }

        let mut pos_state = PosState {
            stm: Color::White,
            color_bbs: [Bitboard::from(0); 2],
            piece_bbs: [Bitboard::from(0); 6],
            castling_rights: Bitboard::from(0),
            en_passant_square: None,
            plies_since_pawn_or_capture: 0,
            move_counter: unsafe { NonZeroU16::new_unchecked(1) },
            last_move: None,
            pt_captured: None,
            checkers: Bitboard::from(0),
            zobrist_hash: 0,
        };

        let pieces_by_rank: Vec<&str> = split_ws[0].split('/').collect();

        if pieces_by_rank.len() != 8 {
            return Err("FEN pieces must be 8 tokens separated by '/'".to_string());
        }

        for (rank_idx, rank_of_pieces) in pieces_by_rank.iter().rev().enumerate() {
            let rank: Rank = unsafe { transmute(rank_idx as u8) };
            let mut file = File::A;

            for chr in rank_of_pieces.chars() {
                // Is this char a digit (in base 10)?
                if let Some(digit) = chr.to_digit(10) {
                    let new_file_idx: u8 = file as u8 + (digit as u8);
                    file = unsafe { transmute(new_file_idx.min(File::H as u8)) }
                }
                // Else we have a piece
                else {
                    let pt: PieceType = PieceType::try_from(chr)
                        .map_err(|_| "Invalid piece character in FEN".to_string())?;

                    let piece_color = [Color::White, Color::Black][chr.is_lowercase() as usize];

                    pos_state.toggle_piece(piece_color, pt, Square::new(file, rank));

                    file = unsafe { transmute((file as u8 + 1).min(File::H as u8)) }
                }
            }
        }

        pos_state.stm = match split_ws[1] {
            "w" | "W" => Ok(Color::White),
            "b" | "B" => Ok(Color::Black),
            _ => Err("Error parsing color to move in FEN"),
        }?;

        if pos_state.stm == Color::Black {
            pos_state.zobrist_hash ^= ZOBRIST_COLOR;
        }

        for chr in split_ws[2].chars() {
            match chr {
                'K' => pos_state.castling_rights |= Bitboard::from(Square::H1),
                'Q' => pos_state.castling_rights |= Bitboard::from(Square::A1),
                'k' => pos_state.castling_rights |= Bitboard::from(Square::H8),
                'q' => pos_state.castling_rights |= Bitboard::from(Square::A8),
                _ => {}
            }
        }

        pos_state.zobrist_hash ^= u64::from(pos_state.castling_rights);

        if let Ok(ep_square) = Square::try_from(split_ws[3]) {
            pos_state.en_passant_square = Some(ep_square);
            pos_state.zobrist_hash ^= ZOBRIST_FILES[ep_square.file()];
        }

        if split_ws.len() > 4 {
            pos_state.plies_since_pawn_or_capture = split_ws[4]
                .parse()
                .map_err(|_| "Error parsing halfmove clock in FEN".to_string())?;
        }

        if split_ws.len() > 5 {
            pos_state.move_counter = split_ws[5]
                .parse()
                .map_err(|_| "Error parsing move counter in FEN".to_string())?;
        }

        pos_state.checkers =
            pos_state.attackers(pos_state.king_square(pos_state.stm)) & pos_state.them();

        debug_assert!(pos_state.checkers.count() <= 2);

        Ok(pos_state)
    }
}

impl PosState {
    pub const fn side_to_move(&self) -> Color {
        self.stm
    }

    pub fn color_bb(&self, color: Color) -> Bitboard {
        self.color_bbs[color]
    }

    pub fn piece_type_bb(&self, pt: PieceType) -> Bitboard {
        self.piece_bbs[pt]
    }

    pub fn piece_bb(&self, color: Color, pt: PieceType) -> Bitboard {
        self.color_bbs[color] & self.piece_bbs[pt]
    }

    pub fn us(&self) -> Bitboard {
        self.color_bbs[self.stm]
    }

    pub fn them(&self) -> Bitboard {
        self.color_bbs[!self.stm]
    }

    pub fn occupancy(&self) -> Bitboard {
        self.color_bbs[Color::White] | self.color_bbs[Color::Black]
    }

    pub const fn castling_rights(&self) -> Bitboard {
        self.castling_rights
    }

    pub const fn en_passant_square(&self) -> Option<Square> {
        self.en_passant_square
    }

    pub const fn plies_since_pawn_or_capture(&self) -> u16 {
        self.plies_since_pawn_or_capture
    }

    pub const fn last_move(&self) -> Option<ChessMove> {
        self.last_move
    }

    pub const fn piece_type_captured(&self) -> Option<PieceType> {
        self.pt_captured
    }

    pub fn piece_type_captured_by(&self, mov: ChessMove) -> Option<PieceType> {
        // En passant
        if mov.piece_type() == PieceType::Pawn && Some(mov.dst()) == self.en_passant_square {
            return Some(PieceType::Pawn);
        }

        self.at(mov.dst())
    }

    pub const fn checkers(&self) -> Bitboard {
        self.checkers
    }

    pub const fn in_check(&self) -> bool {
        !self.checkers.is_empty()
    }

    pub const fn zobrist_hash(&self) -> u64 {
        self.zobrist_hash
    }

    pub fn at(&self, sq: Square) -> Option<PieceType> {
        if !self.occupancy().contains(sq) {
            return None;
        }

        PieceType::iter().find(|&pt| self.piece_bbs[pt].contains(sq))
    }

    pub fn color_at(&self, sq: Square) -> Option<Color> {
        if self.color_bbs[Color::White].contains(sq) {
            Some(Color::White)
        } else if self.color_bbs[Color::Black].contains(sq) {
            Some(Color::Black)
        } else {
            None
        }
    }

    pub fn king_square(&self, color: Color) -> Square {
        debug_assert!(self.piece_bb(color, PieceType::King).count() == 1);

        unsafe {
            self.piece_bb(color, PieceType::King)
                .first_square()
                .unwrap_unchecked()
        }
    }

    pub fn is_capture(&self, mov: ChessMove) -> bool {
        // En passant
        if mov.piece_type() == PieceType::Pawn && Some(mov.dst()) == self.en_passant_square {
            return true;
        }

        self.occupancy().contains(mov.dst())
    }

    pub fn is_noisy_not_underpromotion(&self, mov: ChessMove) -> bool {
        match mov.promotion() {
            Some(PieceType::Queen) => true,
            None => self.is_capture(mov),
            _ => false,
        }
    }

    fn toggle_piece(&mut self, color: Color, pt: PieceType, sq: Square) {
        self.color_bbs[color] ^= Bitboard::from(sq);
        self.piece_bbs[pt] ^= Bitboard::from(sq);
        self.zobrist_hash ^= ZOBRIST_PIECES[color][pt][sq];
    }

    pub fn fen(&self) -> String {
        let mut fen_pieces: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut squares_skipped: i32 = 0;

            for file in File::iter() {
                let sq = Square::new(file, rank);
                let maybe_pt: Option<PieceType> = self.at(sq);

                if maybe_pt.is_none() {
                    squares_skipped += 1;
                    continue;
                }

                if squares_skipped > 0 {
                    fen_pieces[rank] += &(squares_skipped.to_string());
                    squares_skipped = 0;
                }

                let pt: PieceType = unsafe { maybe_pt.unwrap_unchecked() };
                let piece_chr: char = piece_to_char(pt, self.color_at(sq).unwrap());
                fen_pieces[rank].push(piece_chr);
            }

            if squares_skipped > 0 {
                fen_pieces[rank] += &(squares_skipped.to_string());
            }
        }

        let fen_stm = if self.stm == Color::White { "w" } else { "b" };

        let fen_castling_rights = if self.castling_rights.is_empty() {
            "-".to_string()
        } else {
            [
                ('K', Square::H1),
                ('Q', Square::A1),
                ('k', Square::H8),
                ('q', Square::A8),
            ]
            .iter()
            .filter_map(|&(chr, sq)| self.castling_rights.contains(sq).then_some(chr))
            .collect::<String>()
        };

        let fen_ep_square = if let Some(ep_square) = self.en_passant_square {
            ep_square.to_string()
        } else {
            "-".to_string()
        };

        format!(
            "{} {} {} {} {} {}",
            fen_pieces
                .iter()
                .rev()
                .cloned()
                .collect::<Vec<_>>()
                .join("/"),
            fen_stm,
            fen_castling_rights,
            fen_ep_square,
            self.plies_since_pawn_or_capture,
            self.move_counter
        )
    }

    pub fn display(&self) {
        Rank::iter().rev().for_each(|rank| {
            File::iter().for_each(|file| {
                let sq = Square::new(file, rank);

                let chr: char = if let Some(piece_type) = self.at(sq) {
                    piece_to_char(piece_type, self.color_at(sq).unwrap())
                } else {
                    '.'
                };

                print!("{chr}");

                if file == File::H {
                    println!();
                } else {
                    print!(" ");
                }
            })
        });

        println!("\n{}", self.fen());
    }

    pub fn is_insufficient_material(&self) -> bool {
        let num_pieces = self.occupancy().count();

        // KvK
        if num_pieces == 2 {
            return true;
        }

        let num_knights = self.piece_bbs[PieceType::Knight].count();
        let num_bishops = self.piece_bbs[PieceType::Bishop].count();

        // KvN or KvB
        if num_pieces == 3 && (num_knights == 1 || num_bishops == 1) {
            return true;
        }

        // KvNN or NvN or BvB or NvB
        if num_pieces == 4 {
            if num_knights == 2 {
                return true;
            }

            if self.us().count() == 2
                && (num_bishops == 2 || (num_knights == 1 && num_bishops == 1))
            {
                return true;
            }
        }

        false
    }

    pub fn attacks(&self, color: Color, occ: Bitboard) -> Bitboard {
        let mut attacks = Bitboard::from(0);

        for square in self.piece_bb(color, PieceType::Pawn) {
            attacks |= PAWN_ATTACKS[color][square];
        }

        for square in self.piece_bb(color, PieceType::Knight) {
            attacks |= KNIGHT_ATTACKS[square];
        }

        let color_bishops_queens: Bitboard =
            self.piece_bb(color, PieceType::Bishop) | self.piece_bb(color, PieceType::Queen);

        let color_rooks_queens: Bitboard =
            self.piece_bb(color, PieceType::Rook) | self.piece_bb(color, PieceType::Queen);

        for square in color_bishops_queens {
            attacks |= BISHOP_ATTACKS[square].attacks(occ);
        }

        for square in color_rooks_queens {
            attacks |= ROOK_ATTACKS[square].attacks(occ);
        }

        attacks | KING_ATTACKS[self.king_square(color)]
    }

    pub fn attackers(&self, sq: Square) -> Bitboard {
        let mut attackers_bb: Bitboard =
            self.piece_bb(Color::White, PieceType::Pawn) & PAWN_ATTACKS[Color::Black][sq];

        attackers_bb |=
            self.piece_bb(Color::Black, PieceType::Pawn) & PAWN_ATTACKS[Color::White][sq];

        attackers_bb |= self.piece_bbs[PieceType::Knight] & KNIGHT_ATTACKS[sq];

        let bishops_queens: Bitboard =
            self.piece_bbs[PieceType::Bishop] | self.piece_bbs[PieceType::Queen];

        let rooks_queens: Bitboard =
            self.piece_bbs[PieceType::Rook] | self.piece_bbs[PieceType::Queen];

        attackers_bb |= bishops_queens & BISHOP_ATTACKS[sq].attacks(self.occupancy());
        attackers_bb |= rooks_queens & ROOK_ATTACKS[sq].attacks(self.occupancy());

        attackers_bb |= self.piece_bbs[PieceType::King] & KING_ATTACKS[sq];

        attackers_bb
    }

    pub fn pinned(&self) -> (Bitboard, Bitboard) {
        // Bitboards to be calculated and returned
        let mut pinned_orthogonal = Bitboard::from(0);
        let mut pinned_diagonal = Bitboard::from(0);

        // Calculate pinned_orthogonal

        let our_king_sq: Square = self.king_square(self.stm);
        let occ: Bitboard = self.occupancy();

        let rooks_queens = self.piece_bbs[PieceType::Rook] | self.piece_bbs[PieceType::Queen];
        let rook_atks = ROOK_ATTACKS[our_king_sq].attacks(occ);
        let mut new_occ = occ ^ (rook_atks & self.us());
        let xray_rook = rook_atks ^ ROOK_ATTACKS[our_king_sq].attacks(new_occ);

        let pinners_orthogonal: Bitboard = self.them() & rooks_queens & xray_rook;
        for pinner_sq in pinners_orthogonal {
            pinned_orthogonal |= self.us() & BETWEEN_EXCLUSIVE[our_king_sq][pinner_sq];
        }

        // Calculate pinned_diagonal

        let bshps_queens = self.piece_bbs[PieceType::Bishop] | self.piece_bbs[PieceType::Queen];
        let bishop_atks = BISHOP_ATTACKS[our_king_sq].attacks(occ);
        new_occ = occ ^ (bishop_atks & self.us());
        let xray_bishop = bishop_atks ^ BISHOP_ATTACKS[our_king_sq].attacks(new_occ);

        let pinners_diagonal: Bitboard = self.them() & bshps_queens & xray_bishop;
        for pinner_sq in pinners_diagonal {
            pinned_diagonal |= self.us() & BETWEEN_EXCLUSIVE[our_king_sq][pinner_sq];
        }

        (pinned_orthogonal, pinned_diagonal)
    }

    pub fn make_move(&mut self, mov: ChessMove) {
        let src: Square = mov.src();
        let dst: Square = mov.dst();
        let pt_moving: PieceType = mov.piece_type();

        debug_assert!(self.piece_bb(self.stm, pt_moving).contains(src));
        debug_assert!(!self.them().contains(src));
        debug_assert!(!self.us().contains(dst));

        self.pt_captured = self.piece_type_captured_by(mov);
        let is_en_passant = pt_moving == PieceType::Pawn && Some(dst) == self.en_passant_square;

        self.toggle_piece(self.stm, pt_moving, src);

        if let Some(pt_captured) = self.pt_captured {
            let captured_piece_sq: Square = if is_en_passant {
                unsafe { transmute(dst as u8 ^ 8) }
            } else {
                dst
            };

            self.toggle_piece(!self.stm, pt_captured, captured_piece_sq);
        }

        self.toggle_piece(self.stm, mov.promotion().unwrap_or(pt_moving), dst);

        // If castling, move rook
        if mov.is_castling() {
            let (rook_src, rook_dst) = match dst {
                Square::C1 => (Square::A1, Square::D1),
                Square::G1 => (Square::H1, Square::F1),
                Square::C8 => (Square::A8, Square::D8),
                Square::G8 => (Square::H8, Square::F8),
                _ => panic!("Invalid castling king target square"),
            };

            assert!(
                self.castling_rights.contains(rook_src),
                "No castling right for this castling move"
            );

            self.toggle_piece(self.stm, PieceType::Rook, rook_src);
            self.toggle_piece(self.stm, PieceType::Rook, rook_dst);
        }

        self.zobrist_hash ^= u64::from(self.castling_rights); // XOR out old castling rights

        if pt_moving == PieceType::King {
            let mask = match self.stm {
                Color::White => Bitboard::from([Square::A1, Square::H1].as_slice()),
                Color::Black => Bitboard::from([Square::A8, Square::H8].as_slice()),
            };

            self.castling_rights &= !mask;
        } else if self.castling_rights.contains(src) {
            self.castling_rights &= !Bitboard::from(src);
        }

        if self.castling_rights.contains(dst) {
            self.castling_rights &= !Bitboard::from(dst);
        }

        self.zobrist_hash ^= u64::from(self.castling_rights); // XOR in new castling rights

        // If en passant square active, clear it
        if let Some(en_passant_square) = self.en_passant_square {
            self.zobrist_hash ^= ZOBRIST_FILES[en_passant_square.file()];
            self.en_passant_square = None;
        }

        // If pawn double push, create en passant square
        if pt_moving == PieceType::Pawn && src.abs_diff(dst) == 16 {
            let ep_sq: Square = unsafe { transmute(dst as u8 ^ 8) };
            self.en_passant_square = Some(ep_sq);
            self.zobrist_hash ^= ZOBRIST_FILES[ep_sq.file()];
        }

        if pt_moving == PieceType::Pawn || self.pt_captured.is_some() {
            self.plies_since_pawn_or_capture = 0;
        } else {
            self.plies_since_pawn_or_capture += 1;
        }

        self.stm = !self.stm;
        self.zobrist_hash ^= ZOBRIST_COLOR;

        if self.stm == Color::White {
            self.move_counter = self.move_counter.saturating_add(1);
        }

        self.last_move = Some(mov);

        self.checkers = self.attackers(self.king_square(self.stm)) & self.them();
        debug_assert!(self.checkers.count() <= 2);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fen() {
        for (idx, &fen) in [
            FEN_START,
            FEN_KIWIPETE,
            FEN_POS_3,
            FEN_POS_4,
            FEN_POS_5,
            FEN_POS_6,
        ]
        .iter()
        .enumerate()
        {
            let pos_state = PosState::try_from(fen).unwrap();
            let missing_tail = if idx == 1 { " 0 1" } else { "" };
            assert_eq!(pos_state.fen(), fen.to_string() + missing_tail);
        }
    }

    #[test]
    fn test_attacks() {
        let pos_state = PosState::try_from("5k2/2p5/2r5/8/1N6/3K4/8/8 w - - 0 1").unwrap();
        let occ = pos_state.occupancy();

        assert_eq!(
            pos_state.attacks(Color::White, occ),
            Bitboard::from(5532389481728)
        );

        assert_eq!(
            pos_state.attacks(Color::Black, occ),
            Bitboard::from(5797534614998483972)
        );
    }

    #[test]
    fn test_attackers() {
        let pos_state = PosState::try_from(
            "r1b1kbnr/ppp2ppp/2np4/1B2p1q1/3P2P1/1P2PP2/P1P4P/RNBQK1NR b KQkq - 0 5",
        )
        .unwrap();

        assert_eq!(
            pos_state.attackers(Square::F5),
            Bitboard::from(288230652103360512)
        );
    }

    #[test]
    fn test_in_check() {
        let mut pos_state = PosState::try_from("8/8/4k3/8/2R5/1B2K3/8/8 w - - 0 1").unwrap();
        assert!(!pos_state.in_check());

        pos_state.make_move(ChessMove::new(Square::C4, Square::C6, PieceType::Rook));
        assert!(pos_state.in_check());

        pos_state.make_move(ChessMove::new(Square::E6, Square::E7, PieceType::King));
        assert!(!pos_state.in_check());

        pos_state.make_move(ChessMove::new(Square::C6, Square::B6, PieceType::Rook));
        assert!(!pos_state.in_check());

        pos_state.make_move(ChessMove::new(Square::E7, Square::E8, PieceType::King));
        assert!(!pos_state.in_check());

        pos_state.make_move(ChessMove::new(Square::B3, Square::F7, PieceType::Bishop));
        assert!(pos_state.in_check());

        assert_eq!(pos_state.fen(), "4k3/5B2/1R6/8/8/4K3/8/8 b - - 5 3");
    }

    #[test]
    fn test_pinned() {
        let mut pos_state = PosState::try_from(
            "r1b1kbnr/ppp2ppp/2np4/1B2p1q1/3P4/1P2PP2/P1P3PP/RNBQK1NR b KQkq - 0 5",
        )
        .unwrap();

        let (pinned_orthogonal, pinned_diagonal) = pos_state.pinned();

        assert_eq!(
            pinned_orthogonal | pinned_diagonal,
            Bitboard::from(Square::C6)
        );

        pos_state = PosState::try_from("3q3k/2P5/8/5b2/3RN3/3K4/4B3/5q2 w - - 0 1").unwrap();

        let (pinned_orthogonal, pinned_diagonal) = pos_state.pinned();
        assert_eq!(pinned_orthogonal, Bitboard::from(134217728));
        assert_eq!(pinned_diagonal, Bitboard::from(268439552));
    }

    #[test]
    fn test_make_move() {
        let kiwipete = PosState::try_from(FEN_KIWIPETE).unwrap();
        assert_eq!(kiwipete.last_move(), None);

        // Quiet move

        let mut pos_state = kiwipete.clone();
        let mov = ChessMove::new(Square::E2, Square::D3, PieceType::Bishop);
        pos_state.make_move(mov);
        assert_eq!(pos_state.last_move(), Some(mov));

        assert_eq!(
            pos_state.fen(),
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2NB1Q1p/PPPB1PPP/R3K2R b KQkq - 1 1"
        );

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Capture

        pos_state = kiwipete.clone();
        pos_state.make_move(ChessMove::new(Square::F3, Square::F6, PieceType::Queen));

        assert_eq!(
            pos_state.fen(),
            "r3k2r/p1ppqpb1/bn2pQp1/3PN3/1p2P3/2N4p/PPPBBPPP/R3K2R b KQkq - 0 1"
        );

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Non-capture promotion

        pos_state = PosState::try_from("r3k2r/8/8/8/8/8/6p1/4K2R b kq - 0 1").unwrap();

        pos_state.make_move(ChessMove::new_promotion(
            Square::G2,
            Square::G1,
            PieceType::Knight,
        ));

        assert_eq!(pos_state.fen(), "r3k2r/8/8/8/8/8/8/4K1nR w kq - 0 2");

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Capture promotion

        pos_state = PosState::try_from("r3k2r/8/8/8/8/8/6p1/4K2R b kq - 0 1").unwrap();

        pos_state.make_move(ChessMove::new_promotion(
            Square::G2,
            Square::H1,
            PieceType::Knight,
        ));

        assert_eq!(pos_state.fen(), "r3k2r/8/8/8/8/8/8/4K2n w kq - 0 2");

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Castling (black queen side)

        pos_state = PosState::try_from("r3k2r/8/8/8/8/8/6p1/4K2R b kq - 0 1").unwrap();
        pos_state.make_move(ChessMove::new(Square::E8, Square::C8, PieceType::King));

        assert_eq!(pos_state.fen(), "2kr3r/8/8/8/8/8/6p1/4K2R w - - 1 2");

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Moving king loses castling rights

        pos_state =
            PosState::try_from("rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/8/1PPP2PP/RNBQKBNR w KQkq g6 0 5")
                .unwrap();

        pos_state.make_move(ChessMove::new(Square::E1, Square::E2, PieceType::King));

        assert_eq!(
            pos_state.fen(),
            "rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/8/1PPPK1PP/RNBQ1BNR b kq - 1 5"
        );

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Moving rook loses that castling right

        pos_state =
            PosState::try_from("rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/8/1PPP2PP/RNBQKBNR w KQkq g6 0 5")
                .unwrap();

        pos_state.make_move(ChessMove::new(Square::A1, Square::A3, PieceType::Rook));

        assert_eq!(
            pos_state.fen(),
            "rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/R7/1PPP2PP/1NBQKBNR b Kkq - 1 5"
        );

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // Double push creates en passant square

        pos_state = PosState::try_from(FEN_START).unwrap();
        pos_state.make_move(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn));

        assert_eq!(
            pos_state.fen(),
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        );

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );

        // En passant

        pos_state = PosState::try_from("4k3/8/8/2Pp4/8/8/8/4K3 w - d6 0 1").unwrap();
        pos_state.make_move(ChessMove::new(Square::C5, Square::D6, PieceType::Pawn));

        assert_eq!(pos_state.fen(), "4k3/8/3P4/8/8/8/8/4K3 b - - 0 1");

        assert_eq!(
            pos_state,
            PosState::try_from(pos_state.fen().as_str()).unwrap()
        );
    }
}
