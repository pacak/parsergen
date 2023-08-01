use crate::primitives::{unfold_digits, FoldUnfold};
pub(crate) use crate::{primitives::fold_digits, HasWidth, Parsergen};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]

/// Fixed point number with PRE symbols before dot and POST symbols after
/// Similar to [`Cents`] derived implementation will be 2-3x faster
pub struct Fixed<const PRE: usize, const POST: usize, T>(pub T);

macro_rules! from_to {
    ($x:ty) => {
        impl<const PRE: usize, const POST: usize> From<$x> for Fixed<PRE, POST, $x> {
            fn from(value: $x) -> Self {
                Fixed(value)
            }
        }

        impl<const PRE: usize, const POST: usize> From<Fixed<PRE, POST, $x>> for $x {
            fn from(value: Fixed<PRE, POST, $x>) -> Self {
                value.0
            }
        }
    };
}

from_to!(i16);
from_to!(i32);
from_to!(i64);
from_to!(i128);

impl<const PRE: usize, const POST: usize, T> HasWidth for Fixed<PRE, POST, T> {
    const WIDTH: usize = PRE + POST + 1;
}

impl<const WIDTH: usize, const PRE: usize, const POST: usize, T> Parsergen<WIDTH>
    for Fixed<PRE, POST, T>
where
    T: FoldUnfold
        + std::ops::Neg<Output = T>
        + From<i64>
        + std::cmp::PartialOrd
        + std::ops::Div<Output = T>
        + std::ops::Rem
        + std::ops::DivAssign
        + std::convert::TryInto<u8>,
    <T as std::convert::TryInto<u8>>::Error: std::fmt::Debug,
{
    fn des(raw: &[u8; WIDTH]) -> Option<Self>
    where
        Self: Sized,
    {
        let sign = match raw[0] {
            b'-' => Some(-1i64),
            b' ' | b'0' => Some(1),
            _ => None,
        }?;
        let sign = T::from(sign);

        if raw[PRE] != b'.' {
            return None;
        }
        let num1: T = fold_digits(&raw[1..PRE])?;
        let scale = T::from(10i64.pow(POST as u32));
        let num2: T = fold_digits(&raw[PRE + 1..])?;
        Some(Fixed(sign * (num1 * scale + num2)))
    }

    fn ser(&self, raw: &mut [u8; WIDTH]) {
        let zero = T::from(0i64);
        raw[0] = if self.0 > zero { b'0' } else { b'-' };
        let num = if self.0 > zero { self.0 } else { self.0.neg() };
        let scale = T::from(10i64.pow(POST as u32));
        let num1: T = num / scale;
        let num2: T = num % scale;
        raw[PRE] = b'.';
        unfold_digits(num1, &mut raw[1..PRE]);
        unfold_digits(num2, &mut raw[PRE + 1..]);
    }
}

/// Given amount in cents will render it as CNT wide field:
/// Cents<5>(1) is rendered as " 0.01"
/// 0th symbol = sign, ' ' for postive, '-' for negative
/// Similar to [`fixed`] derived implementation will be 2-3x faster
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Cents<const CNT: usize>(pub i64);

impl<const CNT: usize> From<i64> for Cents<CNT> {
    fn from(val: i64) -> Self {
        Cents(val)
    }
}

impl<const CNT: usize> From<Cents<CNT>> for i64 {
    fn from(val: Cents<CNT>) -> Self {
        val.0
    }
}

impl<const WIDTH: usize> HasWidth for Cents<WIDTH> {
    const WIDTH: usize = WIDTH;
}

impl<const CNT: usize> From<Cents<CNT>> for i32 {
    fn from(val: Cents<CNT>) -> Self {
        val.0 as i32
    }
}

impl<const CNT: usize> From<i32> for Cents<CNT> {
    fn from(val: i32) -> Self {
        Cents(val as i64)
    }
}

impl<const WIDTH: usize> Parsergen<WIDTH> for Cents<WIDTH> {
    fn des(raw: &[u8; WIDTH]) -> Option<Self>
    where
        Self: Sized,
    {
        let sign = match raw[0] {
            b' ' | b'0' => 1,
            b'-' => -1,
            _ => return None,
        };
        if raw[WIDTH - 3] != b'.' {
            return None;
        }

        let num1: i64 = fold_digits(&raw[1..WIDTH - 3])?;
        let num2: i64 = fold_digits(&raw[WIDTH - 2..])?;

        Some(Cents(sign * (num1 * 100 + num2)))
    }

    fn ser(&self, raw: &mut [u8; WIDTH]) {
        raw[0] = if self.0 >= 0 { b'0' } else { b'-' };
        let num = self.0.unsigned_abs();
        let num1 = num / 100;
        let num2 = num % 100;
        raw[WIDTH - 3] = b'.';
        unfold_digits(num1, &mut raw[1..WIDTH - 3]);
        unfold_digits(num2, &mut raw[WIDTH - 2..]);
    }
}
