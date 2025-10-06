use std::mem::transmute;

pub const HL_SIZE: usize = 128;
pub const FT_Q: i16 = 181;
pub const VALUE_SCALE: i32 = 400;
pub const POLICY_OUTPUT_SIZE: usize = 6 * 64 * 6; // Piece type moved, dst square, piece type captured

#[repr(C, align(64))]
pub struct Net {
    pub ft_w: [[i16; HL_SIZE]; 768 * 2], // [in_check][piece_color][piece_type][square]
    pub hl_b: [i16; HL_SIZE],
    pub out_w_value: [[f32; HL_SIZE / 2]; 2], // [is_nstm]
    pub out_b_value: f32,
    pub out_w_policy: [[[f32; HL_SIZE / 2]; 2]; POLICY_OUTPUT_SIZE], // [logit_idx][is_nstm]
    pub out_b_policy: [f32; POLICY_OUTPUT_SIZE],                     // [logit_idx]
}

pub const NET: Net = unsafe { transmute(*include_bytes!("../embeds/net.bin")) };
