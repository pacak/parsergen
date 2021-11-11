//! numeric parsing primitives
//!
//! At the moment only serialization bits are used

use crate::primitives::*;
use crate::*;

#[derive(Copy, Clone, Default)]

/// Implements Parsergen for fixed width numbers
pub struct FixedT<T, const WIDTH: usize>(pub T);
impl<T, const WIDTH: usize> HasWidth for FixedT<T, WIDTH> {
    const WIDTH: usize = WIDTH;
}
impl<T, const WIDTH: usize> FixedT<T, WIDTH> {
    pub fn new(val: T) -> Self {
        Self(val)
    }
}

/// used internally to serialize arrays of of items
pub fn ser_fixed_array<T, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    arr: [T; CNT],
    raw: &mut [u8; WIDTH],
) where
    FixedT<T, FWIDTH>: Parsergen<FWIDTH>,
    T: Copy,
{
    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks_mut(FWIDTH).enumerate() {
        let buf = <&mut [u8; FWIDTH]>::try_from(chunk).unwrap();
        FixedT::new(arr[ix]).ser(buf)
    }
}

macro_rules! derive_unsigned {
    ($ty:ty) => {
        impl<const WIDTH: usize> Parsergen<WIDTH> for FixedT<$ty, WIDTH> {
            #[inline(always)]
            fn des(raw: &[u8; WIDTH]) -> Option<Self> {
                Some(FixedT(fold_digits(raw)?))
            }

            fn ser(&self, res: &mut [u8; WIDTH]) {
                unfold_digits(self.0, res);
            }
        }
    };
}

macro_rules! derive_signed {
    ($ty:ty, $ti:ty) => {
        impl<const WIDTH: usize> Parsergen<WIDTH> for FixedT<$ty, WIDTH> {
            #[inline(always)]
            fn des(raw: &[u8; WIDTH]) -> Option<Self> {
                let acc: $ti = fold_digits(&raw[1..])?;
                match raw[0] {
                    b'0' | b' ' | b'+' => Some(Self(acc as $ty)),
                    b'-' => Some(Self(-(acc as $ty))),
                    _ => None,
                }
            }

            fn ser(&self, res: &mut [u8; WIDTH]) {
                let val = self.0;
                res[0] = if val >= 0 { b' ' } else { b'-' };
                unfold_digits(val.abs() as $ti, &mut res[1..]);
            }
        }
    };
}

derive_unsigned!(u8);
derive_unsigned!(u16);
derive_unsigned!(u32);
derive_unsigned!(u64);
derive_unsigned!(u128);
derive_unsigned!(usize);

derive_signed!(i8, u8);
derive_signed!(i16, u16);
derive_signed!(i32, u32);
derive_signed!(i64, u64);
derive_signed!(i128, u128);
derive_signed!(isize, usize);
