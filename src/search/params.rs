use crate::chess::types::PieceType;
use std::array::from_fn;

pub const MAX_DEPTH: i32 = 100;
pub const INF: i32 = 30000;
pub const MIN_MATE_SCORE: i32 = INF - MAX_DEPTH;

pub const CORR_HIST_SIZE: usize = 16384;
pub const HISTORY_MAX: i32 = 16384;

macro_rules! tunable_params {
    { $($name:ident: $type:ident = $value:expr, $min:expr, $max:expr, $step:expr;)* } => {
        #[allow(non_upper_case_globals)]
        mod tuned_params {
            $(
            #[cfg(not(feature = "tune"))]
            const _: () = {
                assert!($value >= $min, stringify!($name));
                assert!($value <= $max, stringify!($name));
                assert!($min < $max, stringify!($name));
            };

            #[cfg(not(feature = "tune"))]
            pub const $name: $type = $value;

            #[cfg(feature = "tune")]
            pub static mut $name: $type = $value;
            )*
        }

        $(
        #[cfg(not(feature = "tune"))]
        #[inline]
        pub const fn $name() -> $type {
            tuned_params::$name
        }

        #[cfg(feature = "tune")]
        #[inline]
        pub fn $name() -> $type {
            unsafe { tuned_params::$name }
        }
        )*

        #[cfg(feature = "tune")]
        pub fn set_tunable_param(name: &str, new_value: &str) -> bool {
            match name {
                $(
                stringify!($name) => unsafe {
                    tuned_params::$name = new_value.parse()
                        .expect("Error parsing value for tunable param");

                    assert!(tuned_params::$name >= $min);
                    assert!(tuned_params::$name <= $max);

                    true
                },
                )*
                _ => false,
            }
        }

        #[cfg(feature = "tune")]
        #[allow(clippy::print_with_newline)]
        pub fn print_params_options() {
            $(
            let format_number = |number| {
                if stringify!($type) == "f32" || stringify!($type) == "f64" {
                    format!("{:.4}", number)
                } else {
                    format!("{}", number)
                }
            };

            let value: $type = unsafe { tuned_params::$name };

            print!("option name {} type string default {} min {} max {}\n",
                stringify!($name),
                format_number(value),
                format_number($min),
                format_number($max)
            );
            )*
        }
    };
}

tunable_params! {
    tm_hard_percent: f64 = 0.75, 0.5, 0.8, 0.05;
    tm_soft_percent: f64 = 0.05, 0.01, 0.25, 0.015;
    tm_nodes_base: f64 = 2.0, 1.5, 2.0, 0.1;
    tm_nodes_mul: f64 = 1.5, 1.0, 1.5, 0.1;
    pawn_value: i32 = 100, 50, 150, 50;
    minor_piece_value: i32 = 300, 200, 400, 50;
    rook_value: i32 = 500, 400, 600, 50;
    queen_value: i32 = 900, 600, 1200, 100;
    asp_initial: i32 = 16, 5, 25, 5;
    rfp_mul: i32 = 75, 30, 130, 10;
    razoring_base: i32 = 350, 200, 600, 40;
    razoring_mul: i32 = 275, 200, 600, 10;
    see_threshold_noisy: i32 = -100, -210, -10, 20;
    see_threshold_quiet: i32 = -50, -210, -10, 20;
    double_ext_margin: i32 = 25, 1, 51, 10;
    lmr_base_quiet: f64 = 0.8, 0.2, 1.2, 0.1;
    lmr_mul_quiet: f64 = 0.4, 0.2, 1.0, 0.1;
    lmr_base_noisy: f64 = 0.8, 0.2, 1.2, 0.1;
    lmr_mul_noisy: f64 = 0.4, 0.2, 1.0, 0.1;
    corr_hist_pk_weight: f32 = 1.0, 0.5, 2.0, 0.15;
    corr_hist_nbrqk_weight: f32 = 1.0, 0.5, 2.0, 0.15;
    corr_hist_last_move_weight: f32 = 1.0, 0.5, 2.0, 0.15;
}

#[cfg(not(feature = "tune"))]
pub const fn get_value(pt: PieceType) -> i32 {
    match pt {
        PieceType::Pawn => pawn_value(),
        PieceType::Knight | PieceType::Bishop => minor_piece_value(),
        PieceType::Rook => rook_value(),
        PieceType::Queen => queen_value(),
        PieceType::King => 0,
    }
}

#[cfg(feature = "tune")]
pub fn get_value(pt: PieceType) -> i32 {
    match pt {
        PieceType::Pawn => pawn_value(),
        PieceType::Knight | PieceType::Bishop => minor_piece_value(),
        PieceType::Rook => rook_value(),
        PieceType::Queen => queen_value(),
        PieceType::King => 0,
    }
}

// [depth][moves_seen][is_noisy]
pub fn get_lmr_table() -> [[[i32; 2]; 256]; MAX_DEPTH as usize + 1] {
    from_fn(|depth| {
        from_fn(|moves_seen| {
            if depth == 0 || moves_seen == 0 {
                [0, 0]
            } else {
                let a = (depth as f64).ln();
                let b = (moves_seen as f64).ln();

                let lmr_quiet = (lmr_base_quiet() + a * b * lmr_mul_quiet()).round() as i32;
                let lmr_noisy = (lmr_base_noisy() + a * b * lmr_mul_noisy()).round() as i32;

                [lmr_quiet, lmr_noisy]
            }
        })
    })
}
