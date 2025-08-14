use std::mem::transmute;

pub const HALF_HL_SIZE: usize = 128;
pub const FT_Q: i16 = 181;
pub const VALUE_SCALE: i32 = 400;

#[repr(C, align(64))]
pub struct Net {
    pub ft_w: [[[[i16; HALF_HL_SIZE]; 64]; 6]; 2], // [piece_color][piece_type][square]
    pub hl_b: [i16; HALF_HL_SIZE],
    pub out_w_value: [[f32; HALF_HL_SIZE]; 2], // [is_nstm]
    pub out_b_value: f32,
    pub out_w_policy: [[[f32; HALF_HL_SIZE]; 2]; 1882], // [move_idx_1882][is_nstm]
    pub out_b_policy: [f32; 1882],                      // [move_idx_1882]
}

pub const NET: Net = unsafe { transmute(*include_bytes!("../embeds/net.bin")) };
