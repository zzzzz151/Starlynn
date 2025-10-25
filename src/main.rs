#![allow(clippy::missing_transmute_annotations)]

mod chess;
mod nn;
mod search;
mod uci;

use core::ops::{Deref, DerefMut};
use search::{thread_data::ThreadData, tt::TT};
use std::num::NonZeroUsize;

#[repr(align(64))]
pub struct Align64<T>(pub T);

impl<T> Deref for Align64<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Align64<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait GetCheckedIfDebug<T> {
    /// Returns a reference to the element at `idx`.
    /// Bounds are checked in debug but not in release.
    ///
    /// # Safety
    ///
    /// UB if `idx >= self.len()`
    fn get_checked_if_debug(&self, idx: usize) -> &T;

    /// Returns a reference to the element at `idx`.
    /// Bounds are checked in debug but not in release.
    ///
    /// # Safety
    ///
    /// UB if `idx >= self.len()`
    fn get_mut_checked_if_debug(&mut self, idx: usize) -> &mut T;
}

impl<T, const N: usize> GetCheckedIfDebug<T> for [T; N] {
    #[inline]
    fn get_checked_if_debug(&self, idx: usize) -> &T {
        debug_assert!(idx < self.len());
        unsafe { self.get_unchecked(idx) }
    }

    #[inline]
    fn get_mut_checked_if_debug(&mut self, idx: usize) -> &mut T {
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
