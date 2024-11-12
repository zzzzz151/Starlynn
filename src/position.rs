use strum::IntoEnumIterator;
use crate::types::{Color, Square, Rank, File, PieceType};
use crate::bitboard::Bitboard;
use crate::utils::{ZOBRIST_COLOR, ZOBRIST_PIECES, ZOBRIST_FILES};
use crate::chess_move::ChessMove;
use crate::attacks::*;

pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Copy, Clone, Debug)]
pub struct Position {
    pieces_bbs: [Bitboard; 6], // [piece_type]
    color_bb: [Bitboard; 2],   // [color]
    stm: Color,
    castling_rights: Bitboard,
    en_passant_square: Option<Square>,
    plies_since_pawn_or_capture: u16,
    move_counter: u16,
    last_move: Option<ChessMove>,
    checkers: Bitboard,
    zobrist_hash: u64
}

#[derive(Debug)]
pub struct InvalidFEN;

impl TryFrom<&str> for Position
{
    type Error = InvalidFEN;

    fn try_from(fen: &str) -> Result<Self, Self::Error>
    {
        let mut pos = Self::default();

        let tokens: Vec<&str> = fen.trim().split_whitespace().collect();
        let pieces_by_rank: Vec<&str> = tokens[0].split('/').collect();

        for (rank_idx, rank_of_pieces) in pieces_by_rank.iter().rev().enumerate()
        {
            let rank: Rank = Rank::from_repr(rank_idx as u8).ok_or(InvalidFEN)?;
            let mut file = File::A;

            for chr in rank_of_pieces.chars()
            {
                // Is this char a digit (in base 10)?
                if let Some(digit) = chr.to_digit(10)
                {
                    let new_file_idx = file as u8 + (digit as u8);
                    file = unsafe { std::mem::transmute(new_file_idx.min(File::H as u8)) }
                }
                // Else we have a piece
                else {
                    let piece_color = if chr.is_uppercase() { Color::White } else { Color::Black };

                    let piece_type = PieceType::try_from(&chr.to_string() as &str)
                        .map_err(|_| InvalidFEN)?;

                    pos.toggle_piece(piece_color, piece_type, Square::from_file_rank(file, rank));

                    file = unsafe { std::mem::transmute((file as u8 + 1).min(File::H as u8)) }
                }
            }
        }

        pos.stm = Color::try_from(tokens[1]).map_err(|_| InvalidFEN)?;

        if pos.stm == Color::Black {
            pos.zobrist_hash ^= ZOBRIST_COLOR;
        }

        for chr in tokens[2].chars()
        {
            match chr {
                'K' => pos.castling_rights |= Bitboard::from(Square::H1),
                'Q' => pos.castling_rights |= Bitboard::from(Square::A1),
                'k' => pos.castling_rights |= Bitboard::from(Square::H8),
                'q' => pos.castling_rights |= Bitboard::from(Square::A8),
                _ => {}
            }
        }

        pos.zobrist_hash ^= u64::from(pos.castling_rights);

        if let Ok(square) = Square::try_from(tokens[3])
        {
            pos.en_passant_square = Some(square);
            pos.zobrist_hash ^= ZOBRIST_FILES[square.file()];
        }

        if tokens.len() > 4 {
            pos.plies_since_pawn_or_capture = tokens[4].parse::<u16>().map_err(|_| InvalidFEN)?;
        }

        if tokens.len() > 5 {
            pos.move_counter = tokens[5].parse::<u16>().map_err(|_| InvalidFEN)?;
        }

        pos.checkers = pos.attackers(pos.king_square(pos.stm)) & pos.them();
        debug_assert!(pos.checkers.count() <= 2);

        Ok(pos)
    }
}

impl Position {
    pub const fn default() -> Self {
        Self {
            pieces_bbs: [Bitboard::EMPTY; 6],
            color_bb: [Bitboard::EMPTY; 2],
            stm: Color::White,
            castling_rights: Bitboard::EMPTY,
            en_passant_square: None,
            plies_since_pawn_or_capture: 0,
            move_counter: 1,
            last_move: None,
            checkers: Bitboard::EMPTY,
            zobrist_hash: 0
        }
    }

    pub const fn stm(&self) -> Color { self.stm }

    pub fn color_bb(&self, color: Color) -> Bitboard {
        self.color_bb[color]
    }

    pub fn piece_type_bb(&self, piece_type: PieceType) -> Bitboard {
         self.pieces_bbs[piece_type]
    }

    pub fn piece_bb(&self, color: Color, piece_type: PieceType) -> Bitboard {
        self.color_bb(color) & self.piece_type_bb(piece_type)
    }

    pub fn us(&self) -> Bitboard {
        self.color_bb[self.stm]
    }

    pub fn them(&self) -> Bitboard {
        self.color_bb[!self.stm]
    }

    pub fn occupancy(&self) -> Bitboard {
        self.color_bb[Color::White] | self.color_bb[Color::Black]
    }

    pub fn has_castling_right(&self, color: Color, queen_side_castle: bool) -> bool
    {
        let required_rook_sq = match (color, queen_side_castle)
        {
            (Color::White, false) => Square::H1,
            (Color::White, true)  => Square::A1,
            (Color::Black, false) => Square::H8,
            (Color::Black, true)  => Square::A8
        };

        let has_right = self.castling_rights.contains_square(required_rook_sq);

        // Assert color has the required rook and king at their required squares
        if cfg!(debug_assertions) {
            let required_king_sq = if color == Color::White { Square::E1 } else { Square::E8 };
            let has_required_rook = self.piece_bb(color, PieceType::Rook).contains_square(required_rook_sq);

            debug_assert!(!has_right || self.king_square(color) == required_king_sq);
            debug_assert!(!has_right || has_required_rook);
        }

        has_right
    }

    pub fn is_castling_legal_move(&self, queen_side_castle: bool, their_attacks: Bitboard) -> bool
    {
        if self.in_check() || !self.has_castling_right(self.stm, queen_side_castle) {
            return false;
        }

        let thru_and_dst_squares = Bitboard::from(match (self.stm, queen_side_castle)
        {
            (Color::White, false) => 96u64,
            (Color::White, true)  => 12u64,
            (Color::Black, false) => 6917529027641081856u64,
            (Color::Black, true)  => 864691128455135232u64
        });

        (thru_and_dst_squares & (self.occupancy() | their_attacks)) == Bitboard::EMPTY
    }

    pub const fn last_move(&self) -> Option<ChessMove> { self.last_move }

    pub fn checkers(&self) -> Bitboard { self.checkers }

    pub fn in_check(&self) -> bool { self.checkers != Bitboard::EMPTY }

    pub const fn zobrist_hash(&self) -> u64 { self.zobrist_hash }

    pub fn at(&self, square: Square) -> Option<PieceType>
    {
        if !self.occupancy().contains_square(square) {
            return None;
        }

        for piece_type in PieceType::iter() {
            if self.pieces_bbs[piece_type].contains_square(square) {
                return Some(piece_type);
            }
        }

        None
    }

    pub fn king_square(&self, color: Color) -> Square
    {
        debug_assert!(self.piece_bb(color, PieceType::King).count() == 1);
        self.piece_bb(color, PieceType::King).first_square().expect("No king")
    }

    fn toggle_piece(&mut self, color: Color, pt: PieceType, square: Square)
    {
        let bb = Bitboard::from(square);
        self.pieces_bbs[pt]  ^= bb;
        self.color_bb[color] ^= bb;
        self.zobrist_hash ^= ZOBRIST_PIECES[color][pt][square];
    }

    pub fn fen(&self) -> String
    {
        let mut pieces_by_rank: [String; 8] = std::array::from_fn(|_| String::from(""));

        for rank in Rank::iter() {
            let mut squares_skipped: u8 = 0;

            for file in File::iter() {
                let square = Square::from_file_rank(file, rank);

                if let Some(piece_type) = self.at(square)
                {
                    if squares_skipped > 0 {
                        pieces_by_rank[rank] += &(squares_skipped.to_string());
                    }

                    if self.color_bb[Color::White].contains_square(square) {
                        pieces_by_rank[rank] += &(piece_type.to_string().to_uppercase());
                    }
                    else {
                        debug_assert!(self.color_bb[Color::Black].contains_square(square));
                        pieces_by_rank[rank] += &(piece_type.to_string().to_lowercase());
                    }

                    squares_skipped = 0;
                }
                else {
                    squares_skipped += 1;
                }
            }

            if squares_skipped > 0 {
                pieces_by_rank[rank] += &(squares_skipped.to_string());
            }
        }

        let mut fen: String = pieces_by_rank
            .iter().rev().cloned().collect::<Vec<_>>().join("/");

        fen += &format!(" {}", self.stm);

        fen += " ";

        if self.castling_rights == Bitboard::EMPTY {
            fen += "-";
        }
        else {
            for castling_right in [
                (Color::White, false, 'K'),
                (Color::White, true, 'Q'),
                (Color::Black, false, 'k'),
                (Color::Black, true, 'q')
            ] {
                if self.has_castling_right(castling_right.0, castling_right.1) {
                    fen += &(castling_right.2).to_string();
                }
            }
        }

        fen += &format!(" {}", match self.en_passant_square {
            Some(square) => square.to_string(),
            None => "-".to_string(),
        });

        format!("{} {} {}", fen, self.plies_since_pawn_or_capture, self.move_counter)
    }

    pub fn display(&self)
    {
        Rank::iter().rev().for_each(|rank| File::iter().for_each(|file|
        {
            let square = Square::from_file_rank(file, rank);

            if let Some(piece_type) = self.at(square)
            {
                if self.color_bb[Color::White].contains_square(square)
                {
                    print!("{}", piece_type.to_string().to_uppercase());
                }
                else {
                    debug_assert!(self.color_bb[Color::Black].contains_square(square));
                    print!("{}", piece_type.to_string().to_lowercase());
                }
            }
            else {
                print!(".");
            }

            if file == File::H {
                print!("\n");
            }
            else {
                print!(" ");
            }
        }));

        println!("{}", self.fen());
    }

    pub fn attacks(&self, color: Color, occupancy: Bitboard) -> Bitboard
    {
        let mut attacks = Bitboard::EMPTY;

        for square in self.piece_bb(color, PieceType::Pawn) {
            attacks |= PAWN_ATTACKS[color][square];
        }

        for square in self.piece_bb(color, PieceType::Knight) {
            attacks |= KNIGHT_ATTACKS[square];
        }

        let bishops_queens = self.piece_bb(color, PieceType::Bishop)
                           | self.piece_bb(color, PieceType::Queen);

        let rooks_queens = self.piece_bb(color, PieceType::Rook)
                         | self.piece_bb(color, PieceType::Queen);

        for square in bishops_queens {
            attacks |= bishop_attacks(square, occupancy);
        }

        for square in rooks_queens {
            attacks |= rook_attacks(square, occupancy);
        }

        attacks | KING_ATTACKS[self.king_square(color)]
    }

    pub fn attackers(&self, square: Square) -> Bitboard
    {
        let mut attackers = self.piece_bb(Color::White, PieceType::Pawn)
                          & PAWN_ATTACKS[Color::Black][square];

        attackers |= self.piece_bb(Color::Black, PieceType::Pawn)
                   & PAWN_ATTACKS[Color::White][square];

        attackers |= self.pieces_bbs[PieceType::Knight] & KNIGHT_ATTACKS[square];

        let bishops_queens = self.pieces_bbs[PieceType::Bishop]
                           | self.pieces_bbs[PieceType::Queen];

        let rooks_queens = self.pieces_bbs[PieceType::Rook]
                         | self.pieces_bbs[PieceType::Queen];

        attackers |= bishops_queens & bishop_attacks(square, self.occupancy());
        attackers |= rooks_queens   & rook_attacks(square, self.occupancy());

        attackers |= self.pieces_bbs[PieceType::King] & KING_ATTACKS[square];

        attackers
    }

    pub fn is_draw(&self, num_moves: usize, hashes_excluding: &Vec<u64>) -> bool
    {
        if num_moves == 0 {
            return !self.in_check();
        }

        // 50 moves rule?
        if self.plies_since_pawn_or_capture >= 100 {
            return true;
        }

        // Only kings?
        if self.occupancy().count() == 2 {
            debug_assert!(self.occupancy() == self.pieces_bbs[PieceType::King]);
            return true;
        }

        // KvN or KvB ?

        let knights_bishops = self.pieces_bbs[PieceType::Knight]
                            | self.pieces_bbs[PieceType::Bishop];

        if self.occupancy().count() == 3 && knights_bishops.count() == 1 {
            return true;
        }

        // Repetition?
        hashes_excluding
            .iter()
            .rev()
            .skip(1)
            .step_by(2)
            .any(|&hash| hash == self.zobrist_hash)
    }

    pub fn make_move(&mut self, mov: ChessMove)
    {
        let from: Square = mov.from();
        let to: Square = mov.to();
        let piece_type = mov.piece_type();
        let mut captured: Option<PieceType> = None;

        debug_assert!(self.at(from).expect("No piece in origin square") == piece_type);
        debug_assert!(self.color_bb[self.stm].contains_square(from));
        debug_assert!(!self.color_bb[self.stm].contains_square(to));

        // If castling move, assert it's legal move
        debug_assert!(
            !mov.is_castling() ||
            self.is_castling_legal_move(from > to, self.attacks(!self.stm, self.occupancy()))
        );

        self.toggle_piece(self.stm, piece_type, from);

        if mov.is_castling()
        {
            let (rook_from, rook_to) = match to {
                Square::C1 => (Square::A1, Square::D1),
                Square::G1 => (Square::H1, Square::F1),
                Square::C8 => (Square::A8, Square::D8),
                Square::G8 => (Square::H8, Square::F8),
                _ => panic!("Invalid castling king target square")
            };

            self.toggle_piece(self.stm, PieceType::King, to);
            self.toggle_piece(self.stm, PieceType::Rook, rook_from);
            self.toggle_piece(self.stm, PieceType::Rook, rook_to);
        }
        // En passant?
        else if piece_type == PieceType::Pawn && Some(to) == self.en_passant_square
        {
            let captured_sq: Square = unsafe { std::mem::transmute(to as u8 ^ 8) };
            self.toggle_piece(!self.stm, PieceType::Pawn, captured_sq);
            self.toggle_piece(self.stm, PieceType::Pawn, to);
            captured = Some(PieceType::Pawn);
        }
        else {
            captured = self.at(to);

            if let Some(captured) = captured {
                self.toggle_piece(!self.stm, captured, to);
            }

            self.toggle_piece(self.stm, mov.promo_piece_type().unwrap_or(piece_type), to);
        }

        self.zobrist_hash ^= u64::from(self.castling_rights); // XOR out old castling rights

        if piece_type == PieceType::King
        {
            let mask = match self.stm {
                Color::White => Bitboard::from(Square::A1) | Bitboard::from(Square::H1),
                Color::Black => Bitboard::from(Square::A8) | Bitboard::from(Square::H8)
            };

            self.castling_rights &= !mask;
        }
        else if self.castling_rights.contains_square(from) {
            self.castling_rights &= !Bitboard::from(from);
        }

        if self.castling_rights.contains_square(to) {
            self.castling_rights &= !Bitboard::from(to);
        }

        self.zobrist_hash ^= u64::from(self.castling_rights); // XOR in new castling rights

        // If en passant square active, clear it
        if let Some(en_passant_square) = self.en_passant_square
        {
            self.zobrist_hash ^= ZOBRIST_FILES[en_passant_square.file()];
            self.en_passant_square = None;
        }

        // If pawn double push, create en passant square
        if piece_type == PieceType::Pawn && from.abs_diff(to) == 16
        {
            self.en_passant_square = unsafe { Some(std::mem::transmute(to as u8 ^ 8)) };
            self.zobrist_hash ^= ZOBRIST_FILES[self.en_passant_square.unwrap().file()];
        }

        if piece_type == PieceType::Pawn || captured != None {
            self.plies_since_pawn_or_capture = 0;
        }
        else {
            self.plies_since_pawn_or_capture += 1;
        }

        self.stm = !self.stm;
        self.zobrist_hash ^= ZOBRIST_COLOR;

        if self.stm == Color::White {
            self.move_counter += 1;
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
    fn test_make_move_fen_zobrist() {
        let kiwipete = Position::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"
        ).unwrap();

        assert_eq!(
            kiwipete.fen(), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        );

        // Quiet move
        let mut pos = kiwipete;
        pos.make_move(ChessMove::new(Square::E2, Square::D3, PieceType::Bishop));
        assert_eq!(pos.fen(), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2NB1Q1p/PPPB1PPP/R3K2R b KQkq - 1 1");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Capture
        pos = kiwipete;
        pos.make_move(ChessMove::new(Square::F3, Square::F6, PieceType::Queen));
        assert_eq!(pos.fen(), "r3k2r/p1ppqpb1/bn2pQp1/3PN3/1p2P3/2N4p/PPPBBPPP/R3K2R b KQkq - 0 1");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Non-capture promotion
        pos = Position::try_from("r3k2r/8/8/8/8/8/6p1/4K2R b kq - 0 1").unwrap();
        pos.make_move(ChessMove::promotion(Square::G2, Square::G1, PieceType::Knight));
        assert_eq!(pos.fen(), "r3k2r/8/8/8/8/8/8/4K1nR w kq - 0 2");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Capture promotion
        pos = Position::try_from("r3k2r/8/8/8/8/8/6p1/4K2R b kq - 0 1").unwrap();
        pos.make_move(ChessMove::promotion(Square::G2, Square::H1, PieceType::Knight));
        assert_eq!(pos.fen(), "r3k2r/8/8/8/8/8/8/4K2n w kq - 0 2");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Castling (black queen side)
        pos = Position::try_from("r3k2r/8/8/8/8/8/6p1/4K2R b kq - 0 1").unwrap();
        pos.make_move(ChessMove::new(Square::E8, Square::C8, PieceType::King));
        assert_eq!(pos.fen(), "2kr3r/8/8/8/8/8/6p1/4K2R w - - 1 2");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Moving king loses castling rights

        pos = Position::try_from(
            "rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/8/1PPP2PP/RNBQKBNR w KQkq g6 0 5"
        ).unwrap();

        pos.make_move(ChessMove::new(Square::E1, Square::E2, PieceType::King));
        assert_eq!(pos.fen(), "rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/8/1PPPK1PP/RNBQ1BNR b kq - 1 5");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Moving rook loses that castling right

        pos = Position::try_from(
            "rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/8/1PPP2PP/RNBQKBNR w KQkq g6 0 5"
        ).unwrap();

        pos.make_move(ChessMove::new(Square::A1, Square::A3, PieceType::Rook));
        assert_eq!(pos.fen(), "rnbqkbnr/pp3p1p/8/2pppPp1/P3P3/R7/1PPP2PP/1NBQKBNR b Kkq - 1 5");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // Double push creates en passant square
        pos = Position::try_from(START_FEN).unwrap();
        pos.make_move(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn));
        assert_eq!(pos.fen(), "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());

        // En passant
        pos = Position::try_from("4k3/8/8/2Pp4/8/8/8/4K3 w - d6 0 1").unwrap();
        pos.make_move(ChessMove::new(Square::C5, Square::D6, PieceType::Pawn));
        assert_eq!(pos.fen(), "4k3/8/3P4/8/8/8/8/4K3 b - - 0 1");
        assert_eq!(pos.zobrist_hash(), Position::try_from(pos.fen().as_str()).unwrap().zobrist_hash());
    }

    #[test]
    fn test_attacks() {
        let pos = Position::try_from("5k2/2p5/2r5/8/1N6/3K4/8/8 w - - 0 1").unwrap();
        assert_eq!(pos.attacks(Color::White, pos.occupancy()), Bitboard::from(5532389481728u64));
        assert_eq!(pos.attacks(Color::Black, pos.occupancy()), Bitboard::from(5797534614998483972u64));
    }

    #[test]
    fn test_attackers() {
        let pos = Position::try_from(
            "r1b1kbnr/ppp2ppp/2np4/1B2p1q1/3P2P1/1P2PP2/P1P4P/RNBQK1NR b KQkq - 0 5"
        ).unwrap();

        assert_eq!(pos.attackers(Square::F5), Bitboard::from(288230652103360512u64));
    }

    #[test]
    fn test_in_check() {
        let mut pos = Position::try_from("8/8/4k3/8/2R5/1B2K3/8/8 w - - 0 1").unwrap();
        assert!(!pos.in_check());

        pos.make_move(ChessMove::new(Square::C4, Square::C6, PieceType::Rook));
        assert!(pos.in_check());

        pos.make_move(ChessMove::new(Square::E6, Square::E7, PieceType::King));
        assert!(!pos.in_check());

        pos.make_move(ChessMove::new(Square::C6, Square::B6, PieceType::Rook));
        assert!(!pos.in_check());

        pos.make_move(ChessMove::new(Square::E7, Square::E8, PieceType::King));
        assert!(!pos.in_check());

        pos.make_move(ChessMove::new(Square::B3, Square::F7, PieceType::Bishop));
        assert!(pos.in_check());

        assert_eq!(pos.fen(), "4k3/5B2/1R6/8/8/4K3/8/8 b - - 5 3");
    }

    #[test]
    fn test_draw() {
        // KvK
        assert!(Position::try_from("4k3/8/8/8/8/8/8/4K3 w - - 0 1")
            .unwrap().is_draw(5, &Vec::new())
        );

        // KvR
        assert!(!Position::try_from("4k3/8/8/8/8/8/8/3RK3 w - - 0 1")
            .unwrap().is_draw(14, &Vec::new())
        );

        // KvR, 50 moves rule
        assert!(Position::try_from("4k3/8/8/8/8/8/8/3RK3 w - - 100 1")
            .unwrap().is_draw(14, &Vec::new())
        );

        // Checkmate, 50 moves rule
        assert!(!Position::try_from("8/8/8/8/8/8/5KQ1/7k b - - 100 1")
            .unwrap().is_draw(0, &Vec::new())
        );

        // Repetition

        let mut pos = Position::try_from(START_FEN).unwrap();
        let mut hashes: Vec<u64> = Vec::new();
        assert!(!pos.is_draw(20, &hashes));

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::B1, Square::C3, PieceType::Knight));
        assert!(!pos.is_draw(20, &hashes));

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::B8, Square::C6, PieceType::Knight));
        assert!(!pos.is_draw(22, &hashes));

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::C3, Square::B1, PieceType::Knight));
        assert!(!pos.is_draw(22, &hashes));

        let pos_before_repetition = pos;
        let hashes_before_repetition = hashes.clone();

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::C6, Square::B8, PieceType::Knight));
        assert!(pos.is_draw(20, &hashes));

        let pos_at_repetition = pos;
        let hashes_at_repetition = hashes.clone();

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::E2, Square::E4, PieceType::Pawn));
        assert!(!pos.is_draw(20, &hashes));

        pos = pos_at_repetition;
        hashes = hashes_at_repetition.clone();

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::B1, Square::C3, PieceType::Knight));
        assert!(pos.is_draw(20, &hashes));

        pos = pos_before_repetition;
        hashes = hashes_before_repetition.clone();

        hashes.push(pos.zobrist_hash());
        pos.make_move(ChessMove::new(Square::E7, Square::E5, PieceType::Pawn));
        assert!(!pos.is_draw(20, &hashes));
    }
}
