use super::attacks::*;
use super::bitboard::Bitboard;
use super::chess_move::ChessMove;
use super::pos_state::PosState;
use super::types::*;
use super::util::{BETWEEN_EXCLUSIVE, LINE_THRU};
use arrayvec::ArrayVec;
use std::mem::transmute;

pub type MovesList = ArrayVec<ChessMove, 256>;

impl PosState {
    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn legal_moves(&self) -> MovesList {
        let mut moves = MovesList::new_const();

        let stm: Color = self.side_to_move();
        let our_king_sq: Square = self.king_square(stm);
        let occ: Bitboard = self.occupancy();
        let enemy_attacks: Bitboard = self.attacks(!stm, occ ^ Bitboard::from(our_king_sq));

        // King moves
        for dst_sq in !self.us() & KING_ATTACKS[our_king_sq] & !enemy_attacks {
            moves.push_unchecked(ChessMove::new(our_king_sq, dst_sq, PieceType::King));
        }

        // If 2 checkers, only king moves are legal
        if self.checkers().count() > 1 {
            return moves;
        }

        // Castling
        if our_king_sq == [Square::E1, Square::E8][stm] && !self.in_check() {
            let short_rook_src: Square = transmute(our_king_sq as u8 + 3);
            let long_rook_src: Square = transmute(our_king_sq as u8 - 4);

            for (rook_src, mul) in [
                (short_rook_src, 1_i32), // Short castling
                (long_rook_src, -1_i32), // Long castling
            ] {
                let has_right = self.castling_rights().contains(rook_src);
                debug_assert!(!has_right || self.piece_bb(stm, PieceType::Rook).contains(rook_src));

                let must_not_attacked_sq1: Square = transmute((our_king_sq as i32 + mul) as u8);
                let must_not_attacked_sq2: Square = transmute((our_king_sq as i32 + 2 * mul) as u8);

                if has_right
                    && (occ & BETWEEN_EXCLUSIVE[our_king_sq][rook_src]).is_empty()
                    && !enemy_attacks.contains(must_not_attacked_sq1)
                    && !enemy_attacks.contains(must_not_attacked_sq2)
                {
                    let king_dst: Square = transmute((our_king_sq as i32 + 2 * mul) as u8);
                    moves.push_unchecked(ChessMove::new(our_king_sq, king_dst, PieceType::King));
                }
            }
        }

        let movable: Bitboard = if let Some(checker_sq) = self.checkers().first_square() {
            let sliders = self.piece_type_bb(PieceType::Bishop)
                | self.piece_type_bb(PieceType::Rook)
                | self.piece_type_bb(PieceType::Queen);

            if sliders.contains(checker_sq) {
                self.checkers() | BETWEEN_EXCLUSIVE[our_king_sq][checker_sq]
            } else {
                self.checkers()
            }
        } else {
            !Bitboard::from(0)
        };

        let (pinned_orthogonal, pinned_diagonal) = self.pinned();
        let pinned = pinned_orthogonal | pinned_diagonal;

        // Pawns moves

        let push_pawn_move = |src: Square, dst: Square, movs: &mut MovesList| {
            // Non-promotion
            if !dst.rank().is_backrank() {
                movs.push_unchecked(ChessMove::new(src, dst, PieceType::Pawn));
                return;
            }

            // Promotion

            movs.push_unchecked(ChessMove::new_promotion(src, dst, PieceType::Queen));

            for promo_pt in [PieceType::Knight, PieceType::Rook, PieceType::Bishop] {
                movs.push_unchecked(ChessMove::new_promotion(src, dst, promo_pt));
            }
        };

        for src_sq in self.piece_bb(stm, PieceType::Pawn) {
            debug_assert!(!src_sq.rank().is_backrank());

            // Pawn's captures

            let mut pawn_captures = PAWN_ATTACKS[stm][src_sq] & movable & self.them();

            if pinned.contains(src_sq) {
                pawn_captures &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in pawn_captures {
                push_pawn_move(src_sq, dst_sq, &mut moves);
            }

            // Pawn's pushes

            if pinned_diagonal.contains(src_sq) {
                continue;
            }

            let mut pin_ray: Bitboard = LINE_THRU[our_king_sq][src_sq];
            pin_ray &= pin_ray << 1;

            // Pawn pinned horizontally?
            if pinned_orthogonal.contains(src_sq) && !pin_ray.is_empty() {
                continue;
            }

            let single_push_sq_idx: u8 = if stm == Color::White {
                src_sq as u8 + 8
            } else {
                src_sq as u8 - 8
            };

            let single_push_sq: Square = transmute(single_push_sq_idx);

            if occ.contains(single_push_sq) {
                continue;
            }

            if movable.contains(single_push_sq) {
                push_pawn_move(src_sq, single_push_sq, &mut moves);
            }

            let start_rank = if stm == Color::White {
                Rank::Rank2
            } else {
                Rank::Rank7
            };

            if src_sq.rank() != start_rank {
                continue;
            }

            let double_push_sq_idx: u8 = if stm == Color::White {
                src_sq as u8 + 16
            } else {
                src_sq as u8 - 16
            };

            let double_push_sq: Square = transmute(double_push_sq_idx);

            if !occ.contains(double_push_sq) && movable.contains(double_push_sq) {
                moves.push_unchecked(ChessMove::new(src_sq, double_push_sq, PieceType::Pawn));
            }
        }

        // En passant moves
        if let Some(en_passant_dst_sq) = self.en_passant_square() {
            let our_ep_pawns: Bitboard =
                self.piece_bb(stm, PieceType::Pawn) & PAWN_ATTACKS[!stm][en_passant_dst_sq];

            let captured_sq: Square = transmute(en_passant_dst_sq as u8 ^ 8);

            for our_pawn_sq in our_ep_pawns {
                let occ_after_ep: Bitboard = occ
                    ^ Bitboard::from(our_pawn_sq)
                    ^ Bitboard::from(en_passant_dst_sq)
                    ^ Bitboard::from(captured_sq);

                let bishops_queens: Bitboard =
                    self.piece_type_bb(PieceType::Bishop) | self.piece_type_bb(PieceType::Queen);

                let rooks_queens: Bitboard =
                    self.piece_type_bb(PieceType::Rook) | self.piece_type_bb(PieceType::Queen);

                let mut slider_attackers_to: Bitboard =
                    bishops_queens & BISHOP_ATTACKS[our_king_sq].attacks(occ_after_ep);

                slider_attackers_to |=
                    rooks_queens & ROOK_ATTACKS[our_king_sq].attacks(occ_after_ep);

                if (self.them() & slider_attackers_to).is_empty() {
                    let mov = ChessMove::new(our_pawn_sq, en_passant_dst_sq, PieceType::Pawn);
                    moves.push_unchecked(mov);
                }
            }
        }

        let mask: Bitboard = !self.us() & movable;

        // Knights moves
        for src_sq in self.piece_bb(stm, PieceType::Knight) & !pinned {
            for dst_sq in KNIGHT_ATTACKS[src_sq] & mask {
                moves.push_unchecked(ChessMove::new(src_sq, dst_sq, PieceType::Knight));
            }
        }

        // Bishops moves
        for src_sq in self.piece_bb(stm, PieceType::Bishop) & !pinned_orthogonal {
            let mut piece_moves: Bitboard = BISHOP_ATTACKS[src_sq].attacks(occ) & mask;

            if pinned_diagonal.contains(src_sq) {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in piece_moves {
                moves.push_unchecked(ChessMove::new(src_sq, dst_sq, PieceType::Bishop));
            }
        }

        // Rooks moves
        for src_sq in self.piece_bb(stm, PieceType::Rook) & !pinned_diagonal {
            let mut piece_moves: Bitboard = ROOK_ATTACKS[src_sq].attacks(occ) & mask;

            if pinned_orthogonal.contains(src_sq) {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in piece_moves {
                moves.push_unchecked(ChessMove::new(src_sq, dst_sq, PieceType::Rook));
            }
        }

        // Queens moves
        for src_sq in self.piece_bb(stm, PieceType::Queen) {
            let bishop_attacks: Bitboard = BISHOP_ATTACKS[src_sq].attacks(occ);
            let rook_attacks: Bitboard = ROOK_ATTACKS[src_sq].attacks(occ);

            let mut piece_moves: Bitboard = (bishop_attacks | rook_attacks) & mask;

            if pinned.contains(src_sq) {
                piece_moves &= LINE_THRU[our_king_sq][src_sq];
            }

            for dst_sq in piece_moves {
                moves.push_unchecked(ChessMove::new(src_sq, dst_sq, PieceType::Queen));
            }
        }

        moves
    }
}
