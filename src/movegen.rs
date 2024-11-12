use arrayvec::ArrayVec;
use crate::position::Position;
use crate::types::{Square, PieceType};
use crate::bitboard::Bitboard;
use crate::utils::BETWEEN_EXCLUSIVE;
use crate::chess_move::ChessMove;
use crate::attacks::*;

impl Position {
    pub fn moves(&self, underpromos: bool) -> ArrayVec<ChessMove, 256>
    {
        let mut moves = ArrayVec::<ChessMove, 256>::new();

        let our_king_sq = self.king_square(self.stm());
        let kingless_occ = self.occupancy() ^ Bitboard::from(our_king_sq);
        let their_attacks = self.attacks(!self.stm(), kingless_occ);

        // King moves
        for square in KING_ATTACKS[our_king_sq] & !self.us() & their_attacks {
            moves.push(ChessMove::new(our_king_sq, square, PieceType::King));
        }

        // If 2 checkers, only king moves are legal
        if self.checkers().count() > 1 {
            return moves;
        }

        // King side castling
        if self.is_castling_legal_move(false, their_attacks)
        {
            let target_sq: Square = unsafe { std::mem::transmute(our_king_sq as u8 + 2) };
            moves.push(ChessMove::new(our_king_sq, target_sq, PieceType::King));
        }

        // Queen side castling
        if self.is_castling_legal_move(true, their_attacks)
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

            if (self.checkers() & sliders) == Bitboard::EMPTY
            {
                let checker_sq = self.checkers().first_square().unwrap();
                self.checkers() | BETWEEN_EXCLUSIVE[our_king_sq][checker_sq]
            }
            else {
                self.checkers()
            }
        };

        let mask = !self.us() & movable;

        for from_sq in self.piece_bb(self.stm(), PieceType::Knight) {
            for to_sq in KNIGHT_ATTACKS[from_sq] & mask {
                moves.push(ChessMove::new(from_sq, to_sq, PieceType::Knight));
            }
        }

        for from_sq in self.piece_bb(self.stm(), PieceType::Bishop) {
            for to_sq in bishop_attacks(from_sq, self.occupancy()) & mask {
                moves.push(ChessMove::new(from_sq, to_sq, PieceType::Bishop));
            }
        }

        moves
    }
}
