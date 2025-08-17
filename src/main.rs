#![allow(clippy::missing_transmute_annotations)]

mod chess;
mod nn;
mod search;
mod uci;

use search::{thread_data::ThreadData, tt::TT};
use std::num::NonZeroUsize;

pub trait GetCheckedIfDebug<T> {
    /// Returns a reference to the element at `idx`.
    /// Bounds are checked in debug but not in release.
    ///
    /// # Safety
    ///
    /// UB if `idx >= self.len()`
    unsafe fn get_checked_if_debug(&self, idx: usize) -> &T;

    /// Returns a reference to the element at `idx`.
    /// Bounds are checked in debug but not in release.
    ///
    /// # Safety
    ///
    /// UB if `idx >= self.len()`
    unsafe fn get_mut_checked_if_debug(&mut self, idx: usize) -> &mut T;
}

impl<T, const N: usize> GetCheckedIfDebug<T> for [T; N] {
    #[inline]
    unsafe fn get_checked_if_debug(&self, idx: usize) -> &T {
        debug_assert!(idx < self.len());
        unsafe { self.get_unchecked(idx) }
    }

    #[inline]
    unsafe fn get_mut_checked_if_debug(&mut self, idx: usize) -> &mut T {
        debug_assert!(idx < self.len());
        unsafe { self.get_unchecked_mut(idx) }
    }
}

fn main() {
    println!("Starlynn by zzzzz");

    let args: Vec<String> = std::env::args().collect();
    let mut td = ThreadData::new();

    let mut tt = TT::new(NonZeroUsize::new(32).unwrap());
    tt.print_size::<false>();

    if args.len() > 1 {
        let input: String = args[1..]
            .iter()
            .map(|arg| arg.trim())
            .collect::<Vec<&str>>()
            .join(" ");

        uci::run_command(&input, &mut td, &mut tt);
        return;
    }

    let mut input = String::new();
    loop {
        std::io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");

        uci::run_command(&input, &mut td, &mut tt);
        input.clear();
    }
}
