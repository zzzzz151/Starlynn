use super::tt_entry::TTEntry;
use std::mem::size_of;
use std::num::NonZeroUsize;
use std::ops::{Index, IndexMut};

// Transposition table
pub struct TT(Vec<TTEntry>);

impl TT {
    pub fn new(mib: NonZeroUsize) -> Self {
        let bytes: usize = mib.get() * 1024 * 1024;
        let tt_len: usize = bytes / size_of::<TTEntry>();
        TT(vec![TTEntry::new(); tt_len])
    }

    pub fn reset_keep_size(&mut self) {
        for entry in &mut self.0 {
            *entry = TTEntry::new();
        }
    }

    pub const fn get_index(&self, zobrist_hash: u64) -> usize {
        let mul: u128 = zobrist_hash as u128 * (self.0.len() as u128);
        (mul >> 64) as usize
    }

    pub fn print_size<const IS_BENCH: bool>(&self) {
        let bytes: usize = size_of::<TTEntry>() * self.0.len();
        let mib: usize = (bytes as f64 / 1024.0 / 1024.0).round() as usize;

        println!(
            "{} TT size {mib} MiB ({} entries)",
            ["info string", "Bench"][IS_BENCH as usize],
            self.0.len()
        );
    }

    pub fn print_fullness<const IS_BENCH: bool>(&self) {
        let num_occupied: usize = self.0.iter().filter(|entry| entry.has_entry()).count();
        let percentage: f64 = num_occupied as f64 * 100.0 / (self.0.len() as f64);

        println!(
            "{}TT fullness: {num_occupied}/{} ({:.1}%)",
            ["", "Bench "][IS_BENCH as usize],
            self.0.len(),
            percentage
        );
    }
}

impl Index<usize> for TT {
    type Output = TTEntry;

    fn index(&self, idx: usize) -> &Self::Output {
        debug_assert!(idx < self.0.len());
        unsafe { self.0.get_unchecked(idx) }
    }
}

impl IndexMut<usize> for TT {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        debug_assert!(idx < self.0.len());
        unsafe { self.0.get_unchecked_mut(idx) }
    }
}
