use strum::IntoEnumIterator;
use crate::types::PieceType;
use crate::position::Position;

impl Position {
    pub fn eval(&self) -> i32
    {
        let mut eval: i32 = 0;

        const VALUES: [i32; 5] = [100, 300, 300, 500, 900];

        for pt in PieceType::iter().take(PieceType::iter().count() - 1)
        {
            let ours_count   = self.piece_bb( self.stm(), pt).count() as i32;
            let theirs_count = self.piece_bb(!self.stm(), pt).count() as i32;

            let piece_value = unsafe { VALUES.get_unchecked(pt as usize) };
            eval += (ours_count - theirs_count) * piece_value;
        }

        let rand = (self.zobrist_hash() % 16) as i32 - 7;
        eval + rand
    }
}
