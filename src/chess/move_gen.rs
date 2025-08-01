use std::mem::transmute;
use arrayvec::ArrayVec;
use super::types::*;
use super::bitboard::Bitboard;
use super::util::{BETWEEN_EXCLUSIVE, LINE_THRU};
use super::chess_move::ChessMove;
use super::attacks::*;
use super::pos_state::PosState;

impl PosState
{
    pub fn legal_moves(&self, underpromos: bool) -> ArrayVec<ChessMove, 256>
    {
        let mut moves = ArrayVec::<ChessMove, 256>::new();

        let our_king_sq = self.king_square(self.stm());
        let occ = self.occupancy();
        let enemy_attacks = self.attacks(!self.stm(), occ ^ Bitboard::from(our_king_sq));

        // King moves
        for dst_sq in !self.us() & KING_ATTACKS[our_king_sq] & !enemy_attacks
        {
            moves.push(ChessMove::new(our_king_sq, dst_sq, PieceType::King));
        }

        // If 2 checkers, only king moves are legal
        if self.checkers().count() > 1
        {
            debug_assert!(self.has_legal_move() == (moves.len() > 0));
            return moves;
        }

        // Castling
        if our_king_sq == [Square::E1, Square::E8][self.stm()] && !self.in_check()
        {
            let short_rook_src: Square = unsafe { transmute(our_king_sq as u8 + 3) };
            let long_rook_src: Square = unsafe { transmute(our_king_sq as u8 - 4) };

            for (rook_src, mul) in [
                (short_rook_src, 1_i32), // Short castling
                (long_rook_src, -1_i32)  // Long castling
            ] {
                let has_right = self.castling_rights().contains(rook_src);

                debug_assert!(
                    !has_right || self.piece_bb(self.stm(), PieceType::Rook).contains(rook_src)
                );

                let must_not_attacked_sq1: Square = unsafe {
                    transmute((our_king_sq as i32 + 1 * mul) as u8)
                };

                let must_not_attacked_sq2: Square = unsafe {
                    transmute((our_king_sq as i32 + 2 * mul) as u8)
                };

                if has_right
                && (occ & BETWEEN_EXCLUSIVE[our_king_sq][rook_src]).is_empty()
                && !enemy_attacks.contains(must_not_attacked_sq1)
                && !enemy_attacks.contains(must_not_attacked_sq2)
                {
                    let king_dst: Square = unsafe {
                        transmute((our_king_sq as i32 + 2 * mul) as u8)
                    };

                    moves.push(ChessMove::new(our_king_sq, king_dst, PieceType::King));
                }
            }
        }

        let movable: Bitboard = if let Some(checker_sq) = self.checkers().first_square()
        {
            let sliders = self.piece_type_bb(PieceType::Bishop)
                        | self.piece_type_bb(PieceType::Rook)
                        | self.piece_type_bb(PieceType::Queen);

            if sliders.contains(checker_sq)
            {
                self.checkers() | BETWEEN_EXCLUSIVE[our_king_sq][checker_sq]
            }
            else {
                self.checkers()
            }
        } else {
            !Bitboard::from(0)
        };

        let (pinned_orthogonal, pinned_diagonal) = self.pinned();
        let pinned = pinned_orthogonal | pinned_diagonal;

        // Pawns moves

        let push_pawn_move = |src: Square, dst: Square, movs: &mut ArrayVec<ChessMove, 256>|
        {
            // Non-promotion
            if !dst.rank().is_backrank()
            {
                movs.push(ChessMove::new(src, dst, PieceType::Pawn));
                return;
            }

            // Promotion

            movs.push(ChessMove::new_promotion(src, dst, PieceType::Queen));

            if underpromos {
                for promo_pt in [PieceType::Knight, PieceType::Rook, PieceType::Bishop]
                {
                    movs.push(ChessMove::new_promotion(src, dst, promo_pt));
                }
            }
        };

        for src_sq in self.piece_bb(self.stm(), PieceType::Pawn)
        {
            debug_assert!(!src_sq.rank().is_backrank());

            // Pawn's captures

            let mut pawn_captures = PAWN_ATTACKS[self.stm()][src_sq] & movable & self.them();

            if pinned.contains(src_sq) { pawn_captures &= LINE_THRU[our_king_sq][src_sq]; }

            for dst_sq in pawn_captures { push_pawn_move(src_sq, dst_sq, &mut moves); }

            // Pawn's pushes

            if pinned_diagonal.contains(src_sq) { continue; }

            let mut pin_ray: Bitboard = LINE_THRU[our_king_sq][src_sq];
            pin_ray &= pin_ray << 1;

            // Pawn pinned horizontally?
            if pinned_orthogonal.contains(src_sq) && !pin_ray.is_empty()
            {
                continue;
            }

            let single_push_sq: Square = unsafe { transmute(
                if self.stm() == Color::White { src_sq as u8 + 8 } else { src_sq as u8 - 8 }
            )};

            if occ.contains(single_push_sq) { continue; }

            if movable.contains(single_push_sq)
            {
                push_pawn_move(src_sq, single_push_sq, &mut moves);
            }

            let start_rank = if self.stm() == Color::White { Rank::Rank2 } else { Rank::Rank7 };

            if src_sq.rank() != start_rank { continue; }

            let double_push_sq: Square = unsafe { transmute(
                if self.stm() == Color::White { src_sq as u8 + 16 } else { src_sq as u8 - 16 }
            )};

            if !occ.contains(double_push_sq) && movable.contains(double_push_sq)
            {
                moves.push(ChessMove::new(src_sq, double_push_sq, PieceType::Pawn));
            }
        }

        // En passant moves
        if let Some(en_passant_dst_sq) = self.en_passant_square()
        {
            let our_ep_pawns = self.piece_bb(self.stm(), PieceType::Pawn)
                             & PAWN_ATTACKS[!self.stm()][en_passant_dst_sq];

            let captured_sq: Square = unsafe {
                transmute(en_passant_dst_sq as u8 ^ 8)
            };

            for our_pawn_sq in our_ep_pawns
            {
                let occ_after_ep = occ
                                 ^ Bitboard::from(our_pawn_sq)
                                 ^ Bitboard::from(en_passant_dst_sq)
                                 ^ Bitboard::from(captured_sq);

                let bishops_queens
                    = self.piece_type_bb(PieceType::Bishop) | self.piece_type_bb(PieceType::Queen);

                let rooks_queens
                    = self.piece_type_bb(PieceType::Rook) | self.piece_type_bb(PieceType::Queen);

                let mut slider_attackers_to
                    = bishops_queens & BISHOP_ATTACKS[our_king_sq].attacks(occ_after_ep);

                slider_attackers_to
                    |= rooks_queens & ROOK_ATTACKS[our_king_sq].attacks(occ_after_ep);

                if (self.them() & slider_attackers_to).is_empty()
                {
                    moves.push(ChessMove::new(our_pawn_sq, en_passant_dst_sq, PieceType::Pawn));
                }
            }
        }

        let mask: Bitboard = !self.us() & movable;

        // Knights moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Knight) & !pinned
        {
            for dst_sq in KNIGHT_ATTACKS[src_sq] & mask
            {
                moves.push(ChessMove::new(src_sq, dst_sq, PieceType::Knight));
            }
        }

        // Bishops moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Bishop) & !pinned_orthogonal
        {
            let mut piece_moves = BISHOP_ATTACKS[src_sq].attacks(occ) & mask;

            if pinned_diagonal.contains(src_sq)
            {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in piece_moves
            {
                moves.push(ChessMove::new(src_sq, dst_sq, PieceType::Bishop));
            }
        }

        // Rooks moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Rook) & !pinned_diagonal
        {
            let mut piece_moves = ROOK_ATTACKS[src_sq].attacks(occ) & mask;

            if pinned_orthogonal.contains(src_sq)
            {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in piece_moves
            {
                moves.push(ChessMove::new(src_sq, dst_sq, PieceType::Rook));
            }
        }

        // Queens moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Queen)
        {
            let bishop_attacks = BISHOP_ATTACKS[src_sq].attacks(occ);
            let rook_attacks = ROOK_ATTACKS[src_sq].attacks(occ);

            let mut piece_moves = (bishop_attacks | rook_attacks) & mask;

            if pinned.contains(src_sq)
            {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in piece_moves
            {
                moves.push(ChessMove::new(src_sq, dst_sq, PieceType::Queen));
            }
        }

        debug_assert!(self.has_legal_move() == (moves.len() > 0));
        moves
    }

    pub fn has_legal_move(&self) -> bool
    {
        let our_king_sq = self.king_square(self.stm());
        let occ = self.occupancy();
        let enemy_attacks = self.attacks(!self.stm(), occ ^ Bitboard::from(our_king_sq));

        // King moves

        let king_moves: Bitboard = !self.us() & KING_ATTACKS[our_king_sq] & !enemy_attacks;

        if !king_moves.is_empty() { return true; }

        // If 2 checkers, only king moves are legal
        if self.checkers().count() > 1 { return false; }

        // Castling
        if our_king_sq == [Square::E1, Square::E8][self.stm()] && !self.in_check()
        {
            let short_rook_src: Square = unsafe { transmute(our_king_sq as u8 + 3) };
            let long_rook_src: Square = unsafe { transmute(our_king_sq as u8 - 4) };

            for (rook_src, mul) in [
                (short_rook_src, 1_i32), // Short castling
                (long_rook_src, -1_i32)  // Long castling
            ] {
                let has_right = self.castling_rights().contains(rook_src);

                debug_assert!(
                    !has_right || self.piece_bb(self.stm(), PieceType::Rook).contains(rook_src)
                );

                let must_not_attacked_sq1: Square = unsafe {
                    transmute((our_king_sq as i32 + 1 * mul) as u8)
                };

                let must_not_attacked_sq2: Square = unsafe {
                    transmute((our_king_sq as i32 + 2 * mul) as u8)
                };

                if has_right
                && (occ & BETWEEN_EXCLUSIVE[our_king_sq][rook_src]).is_empty()
                && !enemy_attacks.contains(must_not_attacked_sq1)
                && !enemy_attacks.contains(must_not_attacked_sq2)
                {
                    return true;
                }
            }
        }

        let movable: Bitboard = if let Some(checker_sq) = self.checkers().first_square()
        {
            let sliders = self.piece_type_bb(PieceType::Bishop)
                        | self.piece_type_bb(PieceType::Rook)
                        | self.piece_type_bb(PieceType::Queen);

            if sliders.contains(checker_sq)
            {
                self.checkers() | BETWEEN_EXCLUSIVE[our_king_sq][checker_sq]
            }
            else {
                self.checkers()
            }
        }
        else {
            !Bitboard::from(0)
        };

        let (pinned_orthogonal, pinned_diagonal) = self.pinned();
        let pinned = pinned_orthogonal | pinned_diagonal;

        let mask: Bitboard = !self.us() & movable;

        // Knights moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Knight) & !pinned
        {
            if !(KNIGHT_ATTACKS[src_sq] & mask).is_empty()
            {
                return true;
            }
        }

        // Bishops moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Bishop) & !pinned_orthogonal
        {
            let mut piece_moves = BISHOP_ATTACKS[src_sq].attacks(occ) & mask;

            if pinned_diagonal.contains(src_sq)
            {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            if !piece_moves.is_empty() { return true; }
        }

        // Rooks moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Rook) & !pinned_diagonal
        {
            let mut piece_moves = ROOK_ATTACKS[src_sq].attacks(occ) & mask;

            if pinned_orthogonal.contains(src_sq)
            {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            if !piece_moves.is_empty() { return true; }
        }

        // Queens moves
        for src_sq in self.piece_bb(self.stm(), PieceType::Queen)
        {
            let bishop_attacks = BISHOP_ATTACKS[src_sq].attacks(occ);
            let rook_attacks = ROOK_ATTACKS[src_sq].attacks(occ);

            let mut piece_moves = (bishop_attacks | rook_attacks) & mask;

            if pinned.contains(src_sq)
            {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            if !piece_moves.is_empty() { return true; }
        }

        // Pawns moves

        for src_sq in self.piece_bb(self.stm(), PieceType::Pawn)
        {
            debug_assert!(!src_sq.rank().is_backrank());

            // Pawn's captures

            let mut pawn_captures = PAWN_ATTACKS[self.stm()][src_sq] & movable & self.them();

            if pinned.contains(src_sq) { pawn_captures &= LINE_THRU[our_king_sq][src_sq]; }

            if !pawn_captures.is_empty() { return true; }

            // Pawn's pushes

            if pinned_diagonal.contains(src_sq) { continue; }

            let mut pin_ray: Bitboard = LINE_THRU[our_king_sq][src_sq];
            pin_ray &= pin_ray << 1;

            // Pawn pinned horizontally?
            if pinned_orthogonal.contains(src_sq) && !pin_ray.is_empty()
            {
                continue;
            }

            let single_push_sq: Square = unsafe { transmute(
                if self.stm() == Color::White { src_sq as u8 + 8 } else { src_sq as u8 - 8 }
            )};

            if occ.contains(single_push_sq) { continue; }

            if movable.contains(single_push_sq) { return true; }

            let start_rank = if self.stm() == Color::White { Rank::Rank2 } else { Rank::Rank7 };

            if src_sq.rank() != start_rank { continue; }

            let double_push_sq: Square = unsafe { transmute(
                if self.stm() == Color::White { src_sq as u8 + 16 } else { src_sq as u8 - 16 }
            )};

            if !occ.contains(double_push_sq) && movable.contains(double_push_sq)
            {
                return true;
            }
        }

        // En passant moves
        if let Some(en_passant_dst_sq) = self.en_passant_square()
        {
            let our_ep_pawns = self.piece_bb(self.stm(), PieceType::Pawn)
                             & PAWN_ATTACKS[!self.stm()][en_passant_dst_sq];

            let captured_sq: Square = unsafe {
                transmute(en_passant_dst_sq as u8 ^ 8)
            };

            for our_pawn_sq in our_ep_pawns
            {
                let occ_after_ep = occ
                                 ^ Bitboard::from(our_pawn_sq)
                                 ^ Bitboard::from(en_passant_dst_sq)
                                 ^ Bitboard::from(captured_sq);

                let bishops_queens
                    = self.piece_type_bb(PieceType::Bishop) | self.piece_type_bb(PieceType::Queen);

                let rooks_queens
                    = self.piece_type_bb(PieceType::Rook) | self.piece_type_bb(PieceType::Queen);

                let mut slider_attackers_to
                    = bishops_queens & BISHOP_ATTACKS[our_king_sq].attacks(occ_after_ep);

                slider_attackers_to
                    |= rooks_queens & ROOK_ATTACKS[our_king_sq].attacks(occ_after_ep);

                if (self.them() & slider_attackers_to).is_empty()
                {
                    return true;
                }
            }
        }

        false
    }

}
