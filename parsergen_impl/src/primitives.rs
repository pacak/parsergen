pub mod numbers {

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
    #[inline(always)]
    pub fn parse_3(d: [u8; 3]) -> Option<u32> {
        parse_4([b'0', d[0], d[1], d[2]])
    }

    #[inline(always)]
    pub fn parse_1(d: [u8; 1]) -> Option<u8> {
        let v = d[0].wrapping_sub(b'0');
        if v < 10 {
            Some(v.into())
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn parse_2(digits: [u8; 2]) -> Option<u16> {
        let v = u16::from_le_bytes(digits);
        const ADD: u16 = u16::from_le_bytes([b'F'; 2]);
        const SUB: u16 = u16::from_le_bytes([b'0'; 2]);
        const MASK: u16 = u16::from_le_bytes([0x80; 2]);
        let a = v.wrapping_add(ADD);
        let mut v = v.wrapping_sub(SUB);
        if (a | v) & MASK == 0 {
            v = (v * 10) + (v >> 8);
            Some(v)
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn parse_4(raw: [u8; 4]) -> Option<u32> {
        let v = u32::from_le_bytes(raw);
        const ADD: u32 = u32::from_le_bytes([b'F'; 4]);
        const SUB: u32 = u32::from_le_bytes([b'0'; 4]);
        const MASK: u32 = u32::from_le_bytes([0x80; 4]);
        let a = v.wrapping_add(ADD);
        let v = v.wrapping_sub(SUB);
        if (a | v) & MASK == 0 {
            let v = (v.wrapping_mul(2561)) >> 8;
            let v = (v & 0x00FF00FF).wrapping_mul(6553601) >> 16;
            Some(v)
        } else {
            None
        }
    }

    #[inline]
    pub fn parse_8(raw: [u8; 8]) -> Option<u32> {
        let v = u64::from_le_bytes(raw);
        const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
        const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
        const MASK: u64 = u64::from_le_bytes([0x80; 8]);
        let a = v.wrapping_add(ADD);
        let v = v.wrapping_sub(SUB);
        if (a | v) & MASK == 0 {
            let v = v.wrapping_mul(2561) >> 8;
            let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(6553601) >> 16;
            let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(42949672960001) >> 32;
            Some(v as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn parse_5a(digits: [u8; 5]) -> Option<u32> {
        fold_digits(&digits)
    }

    #[inline]
    pub fn parse_5b(digits: [u8; 5]) -> Option<u32> {
        let lo = parse_4(digits[0..4].try_into().unwrap())?;
        let hi = fold_digits::<u32>(&digits[4..])?;
        Some(lo + hi * 10000)
    }
}

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

unsafe fn pp(s: &'static str, val: std::arch::x86_64::__m128i) {
    let bs = std::intrinsics::transmute::<std::arch::x86_64::__m128i, [i8; 16]>(val);
    println!("{}: {:?}", s, bs);
}

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

pub fn parse_8c(digits: [u8; 8]) -> Option<u32> {
    let v = u64::from_le_bytes(digits);
    const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
    const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
    const MASK: u64 = u64::from_le_bytes([0x80; 8]);

    let a = v.wrapping_add(ADD);
    let mut v = v.wrapping_sub(SUB);

    if (a | v) & MASK == 0 {
        let mask = 0x0000_00FF_0000_00FFu64;
        const MUL1: u64 = 10u64.pow(2) + (10u64.pow(6) << 32);
        const MUL2: u64 = 10u64.pow(0) + (10u64.pow(4) << 32);

        v = (v * 10) + (v >> 8);
        let v1 = (v & mask).wrapping_mul(MUL1);
        let v2 = ((v >> 16) & mask).wrapping_mul(MUL2);

        let x = (v1.wrapping_add(v2) >> 32) as u32;
        Some(x)
    } else {
        None
    }
}

pub fn parse_8d(digits: [u8; 8]) -> Option<u32> {
    let v = u64::from_le_bytes(digits);
    const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
    const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
    const MASK: u64 = u64::from_le_bytes([0x80; 8]);
    let a = v.wrapping_add(ADD);
    let v = v.wrapping_sub(SUB);
    if (a | v) & MASK == 0 {
        let v = v.wrapping_mul(2561) >> 8;
        let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(6553601) >> 16;
        let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(42949672960001) >> 32;
        Some(v as u32)
    } else {
        None
    }
}

pub fn parse_16c(digits: [u8; 16]) -> Option<u64> {
    let v = u128::from_le_bytes(digits);
    const ADD: u128 = u128::from_le_bytes([b'F'; 16]);
    const SUB: u128 = u128::from_le_bytes([b'0'; 16]);
    const MASK: u128 = u128::from_le_bytes([0x80; 16]);

    let a = v.wrapping_add(ADD);
    let v = v.wrapping_sub(SUB);

    if (a | v) & MASK == 0 {
        let v = v.wrapping_mul(2561) >> 8;
        let v = (v & 0x00FF00FF00FF00FF00FF00FF00FF00FF).wrapping_mul(6553601) >> 16;
        let v = (v & 0x0000FFFF0000FFFF0000FFFF0000FFFF).wrapping_mul(42949672960001) >> 32;
        let v = (v & 0x00000000FFFFFFFF00000000FFFFFFFF).wrapping_mul(1844674407370955161600000001)
            >> 64;
        Some(v as u64)
    } else {
        None
    }
}

// https://johnnylee-sde.github.io/Fast-numeric-string-to-int/
pub fn parse_16d(digits: [u8; 16]) -> Option<u64> {
    let lo = parse_8d(digits[0..8].try_into().unwrap())?; //  + 10_000_000
    let hi = parse_8d(digits[8..16].try_into().unwrap())?; //  + 10_000_000 A
    Some(u64::from(lo) * 10_000_000 + u64::from(hi))
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
