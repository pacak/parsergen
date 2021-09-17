use crate::{Error, Result};
use std::convert::TryInto;

/// Write time given in microseconds into HHMMSSuuuuuu format,
///
/// panics if res is not 12 bytes long
/// panics if duration is above 24 hours
pub fn write_time12(time: u64, res: &mut [u8]) {
    assert!(res.len() == 12);
    assert!(time <= 86400000000); // 24:00:00.000000
    let seconds = time / 1_000_000;
    let mut useconds = time % 1_000_000;
    let h = seconds / 3600;
    let m = (seconds / 60) % 60;
    let s = seconds % 60;
    res[0] = h as u8 / 10 + '0' as u8;
    res[1] = h as u8 % 10 + '0' as u8;
    res[2] = m as u8 / 10 + '0' as u8;
    res[3] = m as u8 % 10 + '0' as u8;
    res[4] = s as u8 / 10 + '0' as u8;
    res[5] = s as u8 % 10 + '0' as u8;
    for r in res[6..].iter_mut().rev() {
        *r = (useconds % 10) as u8 + '0' as u8;
        useconds /= 10;
    }
}

#[test]
fn read_write_time() {
    let mut output = [0u8; 12];
    let expected = b"123456123456";
    let t = read_time12(expected).unwrap();
    write_time12(t, &mut output);
    assert_eq!(expected, &output);
}

/// Parse ASCII represented timestamp HHMMSSuuuuuu
///
/// Panics if raw is not 12 bytes long
/// Returns an unspecified value and sets invalid to true if
/// values re not ASCII digits.
/// Timestamps composed of digits with invalid values for seconds (99) are accepted.
#[cfg(target_feature = "sse2")]
pub fn read_time12(raw: &[u8]) -> Result<u64> {
    read_time12_vec(raw)
}

/// Parse ASCII represented timestamp HHMMSSuuuuuu
///
/// Panics if raw is not 12 characters long
/// Returns an unspecified value and sets invalid to true if
/// values re not ASCII digits.
/// Timestamps composed of digits with invalid values for seconds (99) are accepted.
#[cfg(not(target_feature = "sse2"))]
pub fn read_time12(raw: &[u8]) -> Result<u64> {
    read_time12_native(raw)
}

pub fn read_time12_native(raw: &[u8]) -> Result<u64> {
    let mut digits: [u8; 12] = [0; 12];
    let mut invalid = false;
    for (digit, chr) in digits.iter_mut().zip(raw) {
        *digit = chr.overflowing_sub('0' as u8).0;
        invalid |= *digit > 9;
    }
    if invalid {
        return Err(Error {
            _msg: "Invalid time12",
            _payload: raw,
        });
    }
    let [h1, h2, m1, m2, s1, s2, u1, u2, u3, u4, u5, u6] = digits;
    let h = h1 * 10 + h2;
    let m = m1 * 10 + m2;
    let s = s1 * 10 + s2;
    let u = (((((u1 as u64 * 10 + u2 as u64) * 10 + u3 as u64) * 10 + u4 as u64) * 10 + u5 as u64)
        * 10)
        + u6 as u64;
    Ok(((h as u64 * 60 + m as u64) * 60 + s as u64) * 1_000_000 + u)
}

#[cfg(target_feature = "sse2")]
pub fn read_time12_vec(raw: &[u8]) -> Result<u64> {
    let mut invalid = false;
    let mut digits: [u8; 16] = [0; 16];
    assert!(raw.len() == 12);
    for (digit, &chr) in digits.iter_mut().zip(raw) {
        *digit = chr;
    }

    let r = unsafe { read_time12v(digits, &mut invalid) };
    if invalid {
        return Err(Error {
            _msg: "Invalid time12",
            _payload: raw,
        });
    } else {
        Ok(r)
    }
}

#[cfg(target_feature = "sse2")]
#[test]
fn vectorized_time_parser_works() {
    let raw = b"123446781234";
    let t1 = read_time12(raw);
    let t2 = read_time12_vec(raw);
    assert_eq!(t1.unwrap(), t2.unwrap());
}

#[cfg(target_feature = "sse2")]
unsafe fn read_time12v(raw: [u8; 16], invalid: &mut bool) -> u64 {
    use core::arch::x86_64::*;
    use std::intrinsics::transmute;

    let digits_0 = _mm_set1_epi8('0' as i8);

    // we start from 12 char representation of the timestamp "123456781234"
    let raw = transmute::<[u8; 16], __m128i>(raw);

    // subtract chr '0' and it get's converted to digits [1,2,3,4,5,6,7,8,1,2,3,4]
    let step2 = _mm_sub_epi8(raw, digits_0);

    {
        let digits_9 = _mm_set1_epi8(9);
        // compare every digit with 9, first 12 should be smaller than 9
        // there's no signed compare until avx512 so
        //
        // instead of `(a >= b)` we use `maxu(a, b) == a`
        let good = _mm_cmpeq_epi8(_mm_max_epu8(step2, digits_9), digits_9);
        let mask = _mm_movemask_epi8(good);
        *invalid |= mask != 4095;
    }

    // we need to do \x y -> x * 10 + y on each pair of numbers
    let mask1 = _mm_set1_epi16(0x01_0a);

    // and the result is [12, 34, 56, 78, 12, 34, xx, xx] represented as words (16bit)
    let step3 = _mm_maddubs_epi16(step2, mask1);

    // 1 minute = 60 seconds, 1 second = 100 1/100 seconds plus some smaller bits
    // from the end, groups are - hours, minutes, seconds, 1/100, 1/10000, 1/1000000 of a second
    let mask2 = _mm_set_epi8(0, 0, 0, 0, 0, 1, 0, 100, 0, 1, 0, 100, 0, 1, 0, 60);

    // so we'll get [754, 5678, 1234, 0] stored as dwords (32 bit)
    // those are minutes, 1/100 of a second and 1/10000 of a second
    let step4 = _mm_madd_epi16(step3, mask2);

    // there's no add-multiply instruction on 32 bit words so we pack it down to 16 again
    let zero = _mm_setzero_ps();
    let step5 = _mm_packus_epi32(step4, transmute::<__m128, __m128i>(zero));

    let mask3 = _mm_set_epi16(0, 0, 0, 0, 0, 1, 1, 6000);

    let step6 = _mm_madd_epi16(step5, mask3);
    let res = _mm_cvtsi128_si64(step6) as u64;

    // Add one more multiplication to convert result to microseconds
    return (res >> 32) + (res & 0xFFFFFFFF) * 10000;
}

pub fn fold_digits<T>(digits: &[u8]) -> Result<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8>,
{
    let mut acc = 0.into();
    for c in digits.iter() {
        let d = c.overflowing_sub('0' as u8).0;
        if d > 9 {
            return Err(Error {
                _msg: "invalid digits",
                _payload: digits,
            });
        }
        acc = acc * 10.into() + d.into();
    }
    Ok(acc)
}

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
    let mut invalid_digits = false;

    let r = digits[1..].iter().fold(0.into(), |acc, d| {
        let d = d.overflowing_sub('0' as u8).0;
        invalid_digits |= d > 9;
        acc * 10.into() + d.into()
    });
    match digits[0] as char {
        ' ' if !invalid_digits => Ok(r),
        '-' if !invalid_digits => Ok(-r),
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
        if (b'0'..=b'9').contains(&c) {
            *c = *c - b'0';
        } else if (b'A'..=b'Z').contains(&c) {
            *c = *c - b'A' + 10;
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
            _ => return None,
        }
    }
}

#[test]
fn test_digits() {
    let digits = [3, 10, 12, 15];
    let r = Digits::new(&digits).collect::<Vec<_>>();
    assert_eq!(&r, &[3, 1, 0, 1, 2, 1, 5]);
}
