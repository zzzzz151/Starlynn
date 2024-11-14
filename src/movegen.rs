use arrayvec::ArrayVec;
use crate::types::{Color, Square, Rank, PieceType};
use crate::bitboard::Bitboard;
use crate::utils::{BETWEEN_EXCLUSIVE, LINE_THRU};
use crate::chess_move::ChessMove;
use crate::attacks::*;
use crate::pos_state::PosState;

impl PosState {
    pub fn moves(&mut self, underpromos: bool) -> ArrayVec<ChessMove, 256>
    {
        let mut moves = ArrayVec::<ChessMove, 256>::new();

        let our_king_sq = self.king_square(self.stm());
        let kingless_occ = self.occupancy() ^ Bitboard::from(our_king_sq);
        let their_attacks = self.attacks(!self.stm(), kingless_occ);

        // King moves
        for to_sq in KING_ATTACKS[our_king_sq] & !self.us() & !their_attacks {
            moves.push(ChessMove::new(our_king_sq, to_sq, PieceType::King));
        }

        // If 2 checkers, only king moves are legal
        if self.checkers().count() > 1 { return moves; }

        let is_castling_legal_move = |queen_side_castle: bool| -> bool
        {
            if self.in_check() || !self.has_castling_right(self.stm(), queen_side_castle) {
                return false;
            }

            // If queen side castling, check if square next to rook is occupied
            if queen_side_castle && self.occupancy().contains_square(
                unsafe { std::mem::transmute(our_king_sq as u8 - 3) }
            ) {
                return false;
            }

            let thru_and_dst_squares = Bitboard::from(match (self.stm(), queen_side_castle)
            {
                (Color::White, false) => 96u64,
                (Color::White, true)  => 12u64,
                (Color::Black, false) => 6917529027641081856u64,
                (Color::Black, true)  => 864691128455135232u64
            });

            (thru_and_dst_squares & (self.occupancy() | their_attacks)) == Bitboard::EMPTY
        };

        // King side castling
        if is_castling_legal_move(false)
        {
            let target_sq: Square = unsafe { std::mem::transmute(our_king_sq as u8 + 2) };
            moves.push(ChessMove::new(our_king_sq, target_sq, PieceType::King));
        }

        // Queen side castling
        if is_castling_legal_move(true)
        {
            let target_sq: Square = unsafe { std::mem::transmute(our_king_sq as u8 - 2) };
            moves.push(ChessMove::new(our_king_sq, target_sq, PieceType::King));
        }

        let movable: Bitboard = if self.checkers() == Bitboard::EMPTY {
            Bitboard::FULL
        }
        else {
            let sliders = self.piece_type_bb(PieceType::Bishop)
                        | self.piece_type_bb(PieceType::Rook)
                        | self.piece_type_bb(PieceType::Queen);

            let checker_sq = self.checkers().first_square().unwrap();

            if sliders.contains_square(checker_sq) {
                self.checkers() | BETWEEN_EXCLUSIVE[our_king_sq][checker_sq]
            }
            else {
                self.checkers()
            }
        };

        let mask = movable & !self.us();
        let (pinned_orthogonal, pinned_diagonal) = self.pinned();
        let pinned = pinned_orthogonal | pinned_diagonal;

        for from_sq in self.piece_bb(self.stm(), PieceType::Knight) & !pinned {
            for to_sq in KNIGHT_ATTACKS[from_sq] & mask {
                moves.push(ChessMove::new(from_sq, to_sq, PieceType::Knight));
            }
        }

        // Sliders
        for (piece_type, attacks_fn , (pinned1, pinned2)) in [
            (
                PieceType::Bishop,
                bishop_attacks as fn(Square, Bitboard) -> Bitboard,
                (pinned_orthogonal, pinned_diagonal)
            ),
            (
                PieceType::Rook,
                rook_attacks as fn(Square, Bitboard) -> Bitboard,
                (pinned_diagonal, pinned_orthogonal)
            ),
            (
                PieceType::Queen,
                queen_attacks as fn(Square, Bitboard) -> Bitboard,
                (Bitboard::EMPTY, pinned)
            )
        ] {
            for from_sq in self.piece_bb(self.stm(), piece_type) & !pinned1
            {
                let mut piece_moves = attacks_fn(from_sq, self.occupancy()) & mask;

                if pinned2.contains_square(from_sq) {
                    piece_moves &= LINE_THRU[our_king_sq][from_sq];
                }

                for to_sq in piece_moves {
                    moves.push(ChessMove::new(from_sq, to_sq, piece_type));
                }
            }
        }

        let add_pawn_move = |from: Square, to: Square, movs: &mut ArrayVec<ChessMove, 256>|
        {
            if to.rank().is_backrank() {
                movs.push(ChessMove::promotion(from, to, PieceType::Queen));

                if underpromos {
                    movs.push(ChessMove::promotion(from, to, PieceType::Knight));
                    movs.push(ChessMove::promotion(from, to, PieceType::Rook));
                    movs.push(ChessMove::promotion(from, to, PieceType::Bishop));
                }
            }
            else {
                movs.push(ChessMove::new(from, to, PieceType::Pawn));
            }
        };

        for from_sq in self.piece_bb(self.stm(), PieceType::Pawn)
        {
            debug_assert!(!from_sq.rank().is_backrank());

            // Pawn's captures

            let mut pawn_attacks = PAWN_ATTACKS[self.stm()][from_sq] & movable & self.them();

            if (pinned_orthogonal | pinned_diagonal).contains_square(from_sq) {
                pawn_attacks &= LINE_THRU[our_king_sq][from_sq];
            }

            for to_sq in pawn_attacks {
                add_pawn_move(from_sq, to_sq, &mut moves);
            }

            // Pawn's pushes

            if pinned_diagonal.contains_square(from_sq) {
                continue;
            }

            let mut pin_ray = LINE_THRU[our_king_sq][from_sq];
            pin_ray &= pin_ray << 1;

            let pinned_horizontally =
                pinned_orthogonal.contains_square(from_sq) && pin_ray != Bitboard::EMPTY;

            if pinned_horizontally { continue; }

            let push_to_square = |is_double_push: bool| -> Square
            {
                let to_sq_idx = match (self.stm(), is_double_push)
                {
                    (Color::White, false) => from_sq as u8 + 8,
                    (Color::White, true)  => from_sq as u8 + 16,
                    (Color::Black, false) => from_sq as u8 - 8,
                    (Color::Black, true)  => from_sq as u8 - 16,
                };

                unsafe { std::mem::transmute(to_sq_idx) }
            };

            let single_push_to_sq = push_to_square(false);

            if self.occupancy().contains_square(single_push_to_sq) {
                continue;
            }

            if movable.contains_square(single_push_to_sq) {
                add_pawn_move(from_sq, single_push_to_sq, &mut moves);
            }

            let start_rank = if self.stm() == Color::White { Rank::Rank2 } else { Rank::Rank7 };

            if from_sq.rank() != start_rank { continue; }

            let double_push_to_sq = push_to_square(true);

            if !self.occupancy().contains_square(double_push_to_sq)
            && movable.contains_square(double_push_to_sq)
            {
                add_pawn_move(from_sq, double_push_to_sq, &mut moves);
            }
        }

        // En passant's
        if let Some(en_passant_to_sq) = self.en_passant_square()
        {
            let our_nearby_pawns = self.piece_bb(self.stm(), PieceType::Pawn)
                                 & PAWN_ATTACKS[!self.stm()][en_passant_to_sq];

            let captured_sq: Square = unsafe {
                std::mem::transmute(en_passant_to_sq as u8 ^ 8)
            };

            for our_pawn_sq in our_nearby_pawns {
                // Make en passant move
                self.toggle_piece(self.stm(), PieceType::Pawn, our_pawn_sq);
                self.toggle_piece(self.stm(), PieceType::Pawn, en_passant_to_sq);
                self.toggle_piece(!self.stm(), PieceType::Pawn, captured_sq);

                let checkers = self.attackers(our_king_sq) & self.them();
                debug_assert!(checkers.count() <= 2);

                if checkers == Bitboard::EMPTY {
                    moves.push(ChessMove::new(our_pawn_sq, en_passant_to_sq, PieceType::Pawn));
                }

                // Undo en passant move
                self.toggle_piece(self.stm(), PieceType::Pawn, en_passant_to_sq);
                self.toggle_piece(self.stm(), PieceType::Pawn, our_pawn_sq);
                self.toggle_piece(!self.stm(), PieceType::Pawn, captured_sq);
            }
        }

        debug_assert!((moves.len() > 0) == self.has_move());
        moves
    }

    pub fn has_move(&mut self) -> bool
    {
        let our_king_sq = self.king_square(self.stm());
        let kingless_occ = self.occupancy() ^ Bitboard::from(our_king_sq);
        let their_attacks = self.attacks(!self.stm(), kingless_occ);

        if (KING_ATTACKS[our_king_sq] & !self.us() & !their_attacks) != Bitboard::EMPTY {
            return true;
        }

        if self.checkers().count() > 1 { return false; }

        let is_castling_legal_move = |queen_side_castle: bool| -> bool
        {
            if self.in_check() || !self.has_castling_right(self.stm(), queen_side_castle) {
                return false;
            }

            // If queen side castling, check if square next to rook is occupied
            if queen_side_castle && self.occupancy().contains_square(
                unsafe { std::mem::transmute(our_king_sq as u8 - 3) }
            ) {
                return false;
            }

            let thru_and_dst_squares = Bitboard::from(match (self.stm(), queen_side_castle)
            {
                (Color::White, false) => 96u64,
                (Color::White, true)  => 12u64,
                (Color::Black, false) => 6917529027641081856u64,
                (Color::Black, true)  => 864691128455135232u64
            });

            (thru_and_dst_squares & (self.occupancy() | their_attacks)) == Bitboard::EMPTY
        };

        if is_castling_legal_move(false) || is_castling_legal_move(true) {
            return true;
        }

        let movable: Bitboard = if self.checkers() == Bitboard::EMPTY {
            Bitboard::FULL
        }
        else {
            let sliders = self.piece_type_bb(PieceType::Bishop)
                        | self.piece_type_bb(PieceType::Rook)
                        | self.piece_type_bb(PieceType::Queen);

            let checker_sq = self.checkers().first_square().unwrap();

            if sliders.contains_square(checker_sq) {
                self.checkers() | BETWEEN_EXCLUSIVE[our_king_sq][checker_sq]
            }
            else {
                self.checkers()
            }
        };

        let mask = movable & !self.us();
        let (pinned_orthogonal, pinned_diagonal) = self.pinned();
        let pinned = pinned_orthogonal | pinned_diagonal;

        for from_sq in self.piece_bb(self.stm(), PieceType::Knight) & !pinned {
            if (KNIGHT_ATTACKS[from_sq] & mask) != Bitboard::EMPTY {
                return true;
            }
        }

        // Sliders
        for (piece_type, attacks_fn , (pinned1, pinned2)) in [
            (
                PieceType::Bishop,
                bishop_attacks as fn(Square, Bitboard) -> Bitboard,
                (pinned_orthogonal, pinned_diagonal)
            ),
            (
                PieceType::Rook,
                rook_attacks as fn(Square, Bitboard) -> Bitboard,
                (pinned_diagonal, pinned_orthogonal)
            ),
            (
                PieceType::Queen,
                queen_attacks as fn(Square, Bitboard) -> Bitboard,
                (Bitboard::EMPTY, pinned)
            )
        ] {
            for from_sq in self.piece_bb(self.stm(), piece_type) & !pinned1
            {
                let mut piece_moves = attacks_fn(from_sq, self.occupancy()) & mask;

                if pinned2.contains_square(from_sq) {
                    piece_moves &= LINE_THRU[our_king_sq][from_sq];
                }

                if piece_moves != Bitboard::EMPTY { return true; }
            }
        }

        for from_sq in self.piece_bb(self.stm(), PieceType::Pawn)
        {
            debug_assert!(!from_sq.rank().is_backrank());

            // Pawn's captures

            let mut pawn_attacks = PAWN_ATTACKS[self.stm()][from_sq] & movable & self.them();

            if (pinned_orthogonal | pinned_diagonal).contains_square(from_sq) {
                pawn_attacks &= LINE_THRU[our_king_sq][from_sq];
            }

            if pawn_attacks != Bitboard::EMPTY { return true; }

            // Pawn's pushes

            if pinned_diagonal.contains_square(from_sq) {
                continue;
            }

            let mut pin_ray = LINE_THRU[our_king_sq][from_sq];
            pin_ray &= pin_ray << 1;

            let pinned_horizontally =
                pinned_orthogonal.contains_square(from_sq) && pin_ray != Bitboard::EMPTY;

            if pinned_horizontally { continue; }

            let push_to_square = |is_double_push: bool| -> Square
            {
                let to_sq_idx = match (self.stm(), is_double_push)
                {
                    (Color::White, false) => from_sq as u8 + 8,
                    (Color::White, true)  => from_sq as u8 + 16,
                    (Color::Black, false) => from_sq as u8 - 8,
                    (Color::Black, true)  => from_sq as u8 - 16,
                };

                unsafe { std::mem::transmute(to_sq_idx) }
            };

            let single_push_to_sq = push_to_square(false);

            if self.occupancy().contains_square(single_push_to_sq) {
                continue;
            }

            if movable.contains_square(single_push_to_sq) {
                return true;
            }

            let start_rank = if self.stm() == Color::White { Rank::Rank2 } else { Rank::Rank7 };

            if from_sq.rank() != start_rank { continue; }

            let double_push_to_sq = push_to_square(true);

            if !self.occupancy().contains_square(double_push_to_sq)
            && movable.contains_square(double_push_to_sq)
            {
                return true;
            }
        }

        // En passant's
        if let Some(en_passant_to_sq) = self.en_passant_square()
        {
            let our_nearby_pawns = self.piece_bb(self.stm(), PieceType::Pawn)
                                 & PAWN_ATTACKS[!self.stm()][en_passant_to_sq];

            let captured_sq: Square = unsafe {
                std::mem::transmute(en_passant_to_sq as u8 ^ 8)
            };

            for our_pawn_sq in our_nearby_pawns {
                // Make en passant move
                self.toggle_piece(self.stm(), PieceType::Pawn, our_pawn_sq);
                self.toggle_piece(self.stm(), PieceType::Pawn, en_passant_to_sq);
                self.toggle_piece(!self.stm(), PieceType::Pawn, captured_sq);

                let checkers = self.attackers(our_king_sq) & self.them();
                debug_assert!(checkers.count() <= 2);

                let is_en_passant_legal = checkers == Bitboard::EMPTY;

                // Undo en passant move
                self.toggle_piece(self.stm(), PieceType::Pawn, en_passant_to_sq);
                self.toggle_piece(self.stm(), PieceType::Pawn, our_pawn_sq);
                self.toggle_piece(!self.stm(), PieceType::Pawn, captured_sq);

                if is_en_passant_legal { return true; }
            }
        }

        false
    }

    pub fn pinned(&self) -> (Bitboard, Bitboard)
    {
        // Calculate pinned_orthogonal

        let mut pinned_orthogonal = Bitboard::EMPTY;
        let our_king_sq = self.king_square(self.stm());

        let rooks_queens = self.piece_type_bb(PieceType::Rook)
                         | self.piece_type_bb(PieceType::Queen);

        let rook_atks = rook_attacks(our_king_sq, self.occupancy());
        let new_occ1 = self.occupancy() ^ (rook_atks & self.us());
        let xray_rook = rook_atks ^ rook_attacks(our_king_sq, new_occ1);

        let pinners_orthogonal = rooks_queens & xray_rook & self.them();

        for pinner_sq in pinners_orthogonal {
            pinned_orthogonal |= BETWEEN_EXCLUSIVE[pinner_sq][our_king_sq] & self.us();
        }

        // Calculate pinned_diagonal

        let mut pinned_diagonal = Bitboard::EMPTY;

        let bishops_queens = self.piece_type_bb(PieceType::Bishop)
                           | self.piece_type_bb(PieceType::Queen);

        let bishop_atks = bishop_attacks(our_king_sq, self.occupancy());
        let new_occ2 = self.occupancy() ^ (bishop_atks & self.us());
        let xray_bishop = bishop_atks ^ bishop_attacks(our_king_sq, new_occ2);

        let pinners_diagonal = bishops_queens & xray_bishop & self.them();

        for pinner_sq in pinners_diagonal {
            pinned_diagonal |= BETWEEN_EXCLUSIVE[pinner_sq][our_king_sq] & self.us();
        }

        (pinned_orthogonal, pinned_diagonal)
    }

    pub fn perft(&mut self, depth: u8) -> u64
    {
        if depth == 0 { return 1; }

        let moves = self.moves(true);

        if depth == 1 { return moves.len() as u64; }

        let mut leaves: u64 = 0;

        for mov in moves {
            let mut pos: PosState = *self;
            pos.make_move(mov);
            leaves += pos.perft(depth - 1);
        }

        leaves
    }

    pub fn perft_split(&mut self, depth: u8) -> u64
    {
        if depth == 0 {
            println!("Total: 1");
            return 1u64;
        }

        let moves = self.moves(true);
        let mut total_leaves: u64 = 0;

        for mov in moves {
            let mut pos: PosState = *self;
            pos.make_move(mov);

            let leaves = pos.perft(depth - 1);
            println!("{mov}: {leaves}");
            total_leaves += leaves;
        }

        println!("Total: {total_leaves}");
        total_leaves
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pos_state::START_FEN;

    #[test]
    fn test_pinned() {
        let mut pos = PosState::try_from(
            "r1b1kbnr/ppp2ppp/2np4/1B2p1q1/3P4/1P2PP2/P1P3PP/RNBQK1NR b KQkq - 0 5"
        ).unwrap();

        assert_eq!(pos.pinned().0 | pos.pinned().1, Bitboard::from(Square::C6));

        pos = PosState::try_from("3q3k/2P5/8/5b2/3RN3/3K4/4B3/5q2 w - - 0 1").unwrap();
        let (pinned_orthogonal, pinned_diagonal) = pos.pinned();

        assert_eq!(pinned_orthogonal, Bitboard::from(134217728u64));
        assert_eq!(pinned_diagonal, Bitboard::from(268439552u64));
    }

    #[test]
    fn test_perft() {
        let mut pos: PosState = PosState::try_from(START_FEN).unwrap();

        assert_eq!(pos.perft(1), 20);
        assert_eq!(pos.perft(2), 400);
        assert_eq!(pos.perft(3), 8902);
        assert_eq!(pos.perft(4), 197281);
        assert_eq!(pos.perft(5), 4865609);
        assert_eq!(pos.perft(6), 119060324);

        // Kiwipete
        pos = PosState::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"
        ).unwrap();

        assert_eq!(pos.perft(1), 48);
        assert_eq!(pos.perft(2), 2039);
        assert_eq!(pos.perft(3), 97862);
        assert_eq!(pos.perft(4), 4085603);
        assert_eq!(pos.perft(5), 193690690);
        //assert_eq!(pos.perft(6), 8031647685);
    }
}
