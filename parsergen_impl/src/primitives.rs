use crate::{Error, Result};
use std::convert::TryInto;

#[inline(always)]
pub fn fold_digits<T>(digits: &[u8]) -> Result<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8>,
{
    let mut acc: T = 0.into();
    for d in digits.iter() {
        let d = d.overflowing_sub(b'0').0;
        if d >= 10 {
            return Err(Error {
                _msg: "invalid digits",
                _payload: digits,
            });
        }
        acc = acc * 10.into() + d.into();
    }
    Ok(acc)
}

pub fn fold_u32(digits: &[u8; 12]) -> Result<u32> {
    fold_digits(digits)
}

#[inline(always)]
pub fn unfold_digits<T>(val: T, res: &mut [u8])
where
    T: std::ops::Rem<Output = T>
        + std::ops::DivAssign<T>
        + std::convert::TryInto<u8>
        + Copy
        + From<u8>,
    <T as std::convert::TryInto<u8>>::Error: std::fmt::Debug,
{
    let mut val = val;
    for place in res.iter_mut().rev() {
        *place = b'0' + (val % 10u8.into()).try_into().unwrap();
        val /= 10.into();
    }
}

pub fn fold_signed<T>(digits: &[u8]) -> Result<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8> + std::ops::Neg<Output = T>,
{
    let r = fold_digits(&digits[1..])?;
    match digits[0] as char {
        ' ' | '+' => Ok(r),
        '-' => Ok(-r),
        _ => Err(Error {
            _msg: "invalid digits",
            _payload: digits,
        }),
    }
}

pub trait SignedUnfold {
    fn positive(&self) -> bool;
    fn unfold(self, raw: &mut [u8]);
}

impl SignedUnfold for i8 {
    fn unfold(self, raw: &mut [u8]) {
        unfold_digits::<u8>(self.abs().try_into().unwrap(), raw);
    }
    fn positive(&self) -> bool {
        *self > 0
    }
}

impl SignedUnfold for i16 {
    fn unfold(self, raw: &mut [u8]) {
        unfold_digits::<u16>(self.abs().try_into().unwrap(), raw);
    }

    fn positive(&self) -> bool {
        *self > 0
    }
}

impl SignedUnfold for i32 {
    fn unfold(self, raw: &mut [u8]) {
        unfold_digits::<u32>(self.abs().try_into().unwrap(), raw);
    }

    fn positive(&self) -> bool {
        *self > 0
    }
}

impl SignedUnfold for i64 {
    fn unfold(self, raw: &mut [u8]) {
        unfold_digits::<u64>(self.abs().try_into().unwrap(), raw);
    }

    fn positive(&self) -> bool {
        *self > 0
    }
}

impl SignedUnfold for isize {
    fn unfold(self, raw: &mut [u8]) {
        unfold_digits::<usize>(self.abs().try_into().unwrap(), raw);
    }

    fn positive(&self) -> bool {
        *self > 0
    }
}

pub fn unfold_signed<T>(val: T, res: &mut [u8])
where
    T: SignedUnfold,
{
    res[0] = if val.positive() { b' ' } else { b'-' };
    val.unfold(&mut res[1..]);
}

#[derive(Copy, Clone, Debug)]
pub struct ISIN(pub u64);
/// folds a valid ISIN into u64, see [unfold_isin]
pub fn fold_isin(mut raw: [u8; 12]) -> Option<ISIN> {
    let mut res = 0;
    for c in raw.iter_mut() {
        if (b'0'..=b'9').contains(c) {
            *c -= b'0';
        } else if (b'A'..=b'Z').contains(c) {
            *c -= b'A' - 10;
        } else {
            return None;
        }
    }
    let digits = raw;
    struct Dig {
        sum: u8,
        cnt5: u8,
    }
    let mut sum = (Dig { sum: 0, cnt5: 0 }, Dig { sum: 0, cnt5: 0 });
    for d in Digits::new(&digits) {
        if d >= 5 {
            sum.0.cnt5 += 1;
        }
        sum.0.sum += d;
        sum = (sum.1, sum.0)
    }
    if (sum.0.sum * 2 - sum.0.cnt5 * 9 + sum.1.sum) % 10 != 0 {
        return None;
    }

    for c in digits.iter() {
        res = res * 36 + *c as u64;
    }
    Some(ISIN(res))
}

/// unfolds a valid ISIN into an array of ASCII bytes, see [fold_isin]
pub fn unfold_isin(isin: ISIN) -> [u8; 12] {
    let mut isin = isin.0;
    let mut res = [0; 12];
    for r in res.iter_mut().rev() {
        let c = isin % 36;
        isin /= 36;
        if c < 10 {
            *r = c as u8 + b'0';
        } else {
            *r = c as u8 + b'A' - 10;
        }
    }
    res
}

#[test]
fn test_fold_unfold() {
    let msg = b"AU0000XVGZA3";

    let folded = fold_isin(*msg).unwrap();
    assert_eq!(&unfold_isin(folded), msg);
}

struct Digits<'a>(Option<u8>, &'a [u8]);

impl<'a> Digits<'a> {
    pub fn new(vals: &'a [u8]) -> Self {
        Digits(None, vals)
    }
}
impl<'a> Iterator for Digits<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.0 {
            self.0 = None;
            return Some(n);
        }
        match self.1 {
            [d, rest @ ..] => {
                if *d < 10 {
                    self.1 = rest;
                    Some(*d)
                } else {
                    self.0 = Some(d % 10);
                    self.1 = rest;
                    Some(d / 10)
                }
            }
            _ => None,
        }
    }
}

#[test]
fn test_digits() {
    let digits = [3, 10, 12, 15];
    let r = Digits::new(&digits).collect::<Vec<_>>();
    assert_eq!(&r, &[3, 1, 0, 1, 2, 1, 5]);
}
