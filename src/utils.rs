#[rustfmt::skip]
use std::num::{
    NonZeroI8, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroIsize,
    NonZeroU8, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU128, NonZeroUsize
};

pub trait NonZeroExt {
    type NonZeroType;
    type PrimitiveType;

    fn new_checked_in_debug(n: Self::PrimitiveType) -> Self::NonZeroType;
}

macro_rules! impl_non_zero_ext {
    ($nz_type:ty, $primitive_type:ty) => {
        impl NonZeroExt for $nz_type {
            type NonZeroType = $nz_type;
            type PrimitiveType = $primitive_type;

            #[inline]
            fn new_checked_in_debug(n: $primitive_type) -> Self::NonZeroType {
                debug_assert!(n != 0);
                unsafe { <$nz_type>::new_unchecked(n) }
            }
        }
    };
}

impl_non_zero_ext!(NonZeroI8, i8);
impl_non_zero_ext!(NonZeroI16, i16);
impl_non_zero_ext!(NonZeroI32, i32);
impl_non_zero_ext!(NonZeroI64, i64);
impl_non_zero_ext!(NonZeroI128, i128);
impl_non_zero_ext!(NonZeroIsize, isize);
impl_non_zero_ext!(NonZeroU8, u8);
impl_non_zero_ext!(NonZeroU16, u16);
impl_non_zero_ext!(NonZeroU32, u32);
impl_non_zero_ext!(NonZeroU64, u64);
impl_non_zero_ext!(NonZeroU128, u128);
impl_non_zero_ext!(NonZeroUsize, usize);
