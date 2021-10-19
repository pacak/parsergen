use crate::{Error, Result};

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

/*
unsafe fn pp(s: &'static str, val: std::arch::x86_64::__m128i) {
    let bs = std::intrinsics::transmute::<std::arch::x86_64::__m128i, [i8; 16]>(val);
    println!("{}: {:?}", s, bs);
}*/

#[inline(always)]
pub fn decimal_mask(raw: [u8; 16]) -> i32 {
    unsafe {
        use core::arch::x86_64::*;
        use std::intrinsics::transmute;
        // take a digit, subtract with overflow such that
        // 0 corresponds to -128 .. 9 corresponds to -128 + 9
        // return a mask of values less than -128 + 10
        // those will be valid digits
        let ascii_digits = transmute::<[u8; 16], __m128i>(raw);
        let offset = _mm_set1_epi8((b'0' + 128) as i8);
        let shifted_digits = _mm_sub_epi8(ascii_digits, offset);
        let high_bound = _mm_set1_epi8(-128 + 10);
        let mask = _mm_cmpgt_epi8(high_bound, shifted_digits);
        _mm_movemask_epi8(mask)
    }
}

#[test]
fn test_decimal_mask() {
    let mask = decimal_mask(*b"/0123456789:;<=>");
    assert_eq!(0b00000_11111_11111_0, mask, "{:b}", mask);
}

#[inline(always)]
#[cfg(target_feature = "sse2")]
pub fn fold_digits_vec<T>(digits: &[u8]) -> Result<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8>,
{
    let mut acc: T = 0.into();
    for chunk in digits.chunks(16) {
        let mut mask = [0u8; 16];
        let width = chunk.len();
        mask[..width].copy_from_slice(chunk);
        let masked = decimal_mask(mask);
        if masked != (1 << width) - 1 {
            return Err(Error {
                _msg: "invalid digits",
                _payload: digits,
            });
        }
    }
    for d in digits.iter() {
        let d = d.overflowing_sub(b'0').0;
        acc = acc * 10.into() + d.into();
    }
    Ok(acc)
}

#[cfg(not(target_feature = "sse2"))]
#[inline(always)]
pub fn fold_digits_vec<T>(digits: &[u8]) -> Result<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8>,
{
    fold_digits::<T>(digits)
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
        '0' | ' ' | '+' => Ok(r),
        '-' => Ok(-r),
        _ => Err(Error {
            _msg: "invalid digits (sign)",
            _payload: digits,
        }),
    }
}

pub fn fold_signed_vec<T, const WIDTH: usize>(digits: &[u8]) -> Result<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8> + std::ops::Neg<Output = T>,
{
    let r = fold_digits_vec::<T>(&digits[1..])?;
    match digits[0] as char {
        '0' | ' ' | '+' => Ok(r),
        '-' => Ok(-r),
        _ => Err(Error {
            _msg: "invalid digits (sign)",
            _payload: digits,
        }),
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ISIN(pub u64);
/// folds a valid ISIN into u64, see [unfold_isin]
pub fn fold_isin(raw: [u8; 12]) -> Option<ISIN> {
    let mut res: u64 = 0;
    let mut mixer = luhn3::Mixer::default();
    for c in raw.iter() {
        match c {
            b'0'..=b'9' => {
                let c = c - b'0';
                mixer.push(c);
                res = res * 36 + c as u64;
            }
            b'A'..=b'Z' => {
                let c = c - b'A' + 10;
                mixer.push(c / 10);
                mixer.push(c % 10);
                res = res * 36 + c as u64;
            }
            _ => return None,
        }
    }
    if mixer.valid() {
        Some(ISIN(res))
    } else {
        None
    }
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
