use crate::primitives::unfold_digits;
pub(crate) use crate::{primitives::fold_digits, HasWidth, Parsergen};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]

/// Given amount in cents will render it as CNT wide field:
/// Cents<5>(1) is rendered as " 0.01"
/// 0th symbol = sign, ' ' for postive, '-' for negative
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

        let num1: u64 = fold_digits(&raw[1..WIDTH - 3])?;
        let num2: u64 = fold_digits(&raw[WIDTH - 2..])?;

        Some(Cents(sign * (num1 as i64 * 100 + num2 as i64)))
    }

    fn ser(&self, raw: &mut [u8; WIDTH]) {
        if self.0 >= 0 {
            raw[0] = b'0';
        } else {
            raw[0] = b'-';
        }
        let num = self.0.abs() as u64;
        let num1 = num / 100;
        let num2 = num % 100;
        raw[WIDTH - 3] = b'.';
        unfold_digits(num1, &mut raw[1..WIDTH - 3]);
        unfold_digits(num2, &mut raw[WIDTH - 2..]);
    }
}
