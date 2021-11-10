#[inline(always)]
pub fn fold_digits<T>(digits: &[u8]) -> Option<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8>,
{
    let mut acc: T = 0.into();
    for d in digits.iter() {
        let d = d.overflowing_sub(b'0').0;
        if d >= 10 {
            return None;
        }
        acc = acc * 10.into() + d.into();
    }
    Some(acc)
}

/*
unsafe fn pp(s: &'static str, val: std::arch::x86_64::__m128i) {
    let bs = std::intrinsics::transmute::<std::arch::x86_64::__m128i, [i8; 16]>(val);
    println!("{}: {:?}", s, bs);
}*/

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

#[inline(always)]
pub fn read_decimal(raw: [u8; 16]) -> (i32, [u64; 2]) {
    unsafe {
        use core::arch::x86_64::*;
        use std::intrinsics::transmute;
        let ascii_digits = transmute::<[u8; 16], __m128i>(raw);
        let offset = _mm_set1_epi8((b'0' + 128) as i8);
        let shifted_digits = _mm_sub_epi8(ascii_digits, offset);
        let high_bound = _mm_set1_epi8(-128 + 10);
        let mask = _mm_cmpgt_epi8(high_bound, shifted_digits);
        let digits_mask = _mm_movemask_epi8(mask);

        let chunk = transmute(raw);
        let zeros = _mm_set1_epi8(b'0' as i8);
        let chunk = _mm_sub_epi8(chunk, zeros);

        //        let mult = _mm_set_epi8(1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10);
        let mult = _mm_set1_epi16(0x010a);
        let chunk = _mm_maddubs_epi16(chunk, mult);

        let mult = _mm_set_epi16(1, 100, 1, 100, 1, 100, 1, 100);
        let chunk = _mm_madd_epi16(chunk, mult);

        let chunk = _mm_packus_epi32(chunk, chunk);
        let mult = _mm_set_epi16(0, 0, 0, 0, 1, 10000, 1, 10000);
        let chunk = _mm_madd_epi16(chunk, mult);

        let re: [u32; 4] = transmute(chunk);
        let hi = re[0] as u64;
        let lo = re[1] as u64;

        (digits_mask, [hi, lo])
    }
}

#[test]
fn test_read_decimal() {
    let digit = b"0000000120001011";
    let (mask, [hi, lo]) = read_decimal(*digit);
    assert_eq!(mask, 65535);
    assert_eq!(hi, 1);
    assert_eq!(lo, 20001011);
}

#[test]
fn test_decimal_mask() {
    let mask = decimal_mask(*b"/0123456789:;<=>");
    assert_eq!(0b00000_11111_11111_0, mask, "{:b}", mask);
}

#[inline(always)]
#[cfg(target_feature = "sse2")]
pub fn fold_digits_vec<T>(digits: &[u8]) -> Option<T>
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
            return None;
        }
    }
    for d in digits.iter() {
        let d = d.overflowing_sub(b'0').0;
        acc = acc * 10.into() + d.into();
    }
    Some(acc)
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

pub fn fold_signed<T>(digits: &[u8]) -> Option<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8> + std::ops::Neg<Output = T>,
{
    let r = fold_digits(&digits[1..])?;
    match digits[0] as char {
        '0' | ' ' | '+' => Some(r),
        '-' => Some(-r),
        _ => None,
    }
}

pub fn fold_signed_vec<T, const WIDTH: usize>(digits: &[u8]) -> Option<T>
where
    T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8> + std::ops::Neg<Output = T>,
{
    let r = fold_digits_vec::<T>(&digits[1..])?;
    match digits[0] as char {
        '0' | ' ' | '+' => Some(r),
        '-' => Some(-r),
        _ => None,
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ISIN(pub u64);
/// folds a valid ISIN into u64, see [unfold_isin]
pub fn fold_isin(raw: [u8; 12]) -> Option<ISIN> {
    let mut res: u64 = 0;

    for c in raw.iter() {
        res *= 36;
        res += match c {
            b'0'..=b'9' => (c - b'0') as u64,
            b'A'..=b'Z' => (c - b'A' + 10) as u64,
            _ => return None,
        }
    }
    if luhn3::valid(&raw) {
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
