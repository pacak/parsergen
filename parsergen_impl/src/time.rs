use crate::{
    primitives::{fold_digits, unfold_digits},
    Error, Result,
};

const NS_IN_SEC: u64 = 1_000_000_000;

/// Parse time in HHMMSS format
///
/// Parses time given as HHMMSS into nanoseconds since the beginning of the day
/// `HHMMSS` means 6 digits, no extra symbols. There's no additional checks on
/// time validity: `read_time6` will accept `25:99:99`.
/// # Examples
/// ```rust
/// use parsergen::time::*;
/// let input = b"000001";
/// let r = read_time6(input).unwrap();
/// assert_eq!(r, 1_000_000_000);
/// ```
pub fn read_time6(raw: &[u8]) -> Result<u64> {
    if raw.len() != 6 {
        return Err(Error {
            _msg: "Invalid time6",
            _payload: raw,
        });
    }
    let h: u64 = fold_digits(&raw[0..2])?;
    let m: u64 = fold_digits(&raw[2..4])?;
    let s: u64 = fold_digits(&raw[4..6])?;
    Ok(((h * 60 + m) * 60 + s) * NS_IN_SEC)
}

/// Parse time in HHMMSSss format
///
/// Parses time given as HHMMSSqq into nanoseconds since the beginning of the day
/// `HHMMSSqq` means 8 digits, no extra symbols. There's no additional checks on
/// time validity: `read_time6` will accept `25:99:99.22`.
/// # Examples
/// ```rust
/// use parsergen::time::*;
/// let input = b"00000100";
/// let r = read_time6(input).unwrap();
/// assert_eq!(r, 1_000_000_000);
/// ```
pub fn read_time8(raw: &[u8]) -> Result<u64> {
    if raw.len() != 8 {
        return Err(Error {
            _msg: "Invalid time8",
            _payload: raw,
        });
    }
    let t = read_time6(&raw[0..6])?;
    let f: u64 = fold_digits(&raw[6..8])?;
    Ok(t + f * 100_000)
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

/// Write time given in microseconds into HHMMSSuu format,
///
/// panics if res is not 8 bytes long
/// panics if duration is above 24 hours
pub fn write_time6(time: u64, res: &mut [u8]) {
    assert!(res.len() == 6);
    assert!(time <= 86400 * NS_IN_SEC); // 24:00:00.000000
    let seconds = time / NS_IN_SEC;
    let h = seconds / 3600;
    let m = (seconds / 60) % 60;
    let s = seconds % 60;
    unfold_digits(h, &mut res[0..2]);
    unfold_digits(m, &mut res[2..4]);
    unfold_digits(s, &mut res[4..6]);
}

/// Write time given in microseconds into HHMMSSuu format,
///
/// panics if res is not 8 bytes long
pub fn write_time8(time: u64, res: &mut [u8]) {
    assert!(res.len() == 8);
    let seconds = time / NS_IN_SEC;
    let n = time % NS_IN_SEC / 100_000;
    let h = seconds / 3600;
    let m = (seconds / 60) % 60;
    let s = seconds % 60;
    unfold_digits(h, &mut res[0..2]);
    unfold_digits(m, &mut res[2..4]);
    unfold_digits(s, &mut res[4..6]);
    unfold_digits(n, &mut res[6..8]);
}

/// Write time given in microseconds into HHMMSSuuuuuu format,
///
/// panics if res is not 12 bytes long
pub fn write_time12(time: u64, res: &mut [u8]) {
    assert!(res.len() == 12);
    let seconds = time / NS_IN_SEC;
    let n = time % NS_IN_SEC / 1000;
    let h = seconds / 3600;
    let m = (seconds / 60) % 60;
    let s = seconds % 60;
    unfold_digits(h, &mut res[0..2]);
    unfold_digits(m, &mut res[2..4]);
    unfold_digits(s, &mut res[4..6]);
    unfold_digits(n, &mut res[6..12]);
}

#[test]
fn read_time_check() {
    let t6 = read_time6(b"012345").unwrap();
    assert_eq!(3600 + 23 * 60 + 45, t6 / NS_IN_SEC);
    let t8 = read_time8(b"01234500").unwrap();
    assert_eq!(t6, t8);
    let t12 = read_time12(b"012345000000").unwrap();
    assert_eq!(t6, t12);
}

#[test]
fn read_write_time6() {
    let mut output = [0u8; 6];
    let expected = b"123456";
    let t = read_time6(expected).unwrap();
    assert_eq!(12 * 3600 + 34 * 60 + 56, t / NS_IN_SEC);
    write_time6(t, &mut output);
    assert_eq!(expected, &output);
}

#[test]
fn read_write_time8() {
    let mut output = [0u8; 8];
    let expected = b"12345612";
    let t = read_time8(expected).unwrap();
    assert_eq!(12 * 3600 + 34 * 60 + 56, t / NS_IN_SEC);
    assert_eq!(1_200_000, t % NS_IN_SEC);
    write_time8(t, &mut output);
    assert_eq!(expected, &output);
}

#[test]
fn read_write_time12() {
    assert_eq!(1_000, read_time12(b"000000000001").unwrap(), "1us");
    assert_eq!(1_000_000, read_time12(b"000000001000").unwrap(), "1ms");
    assert_eq!(
        NS_IN_SEC / 10,
        read_time12(b"000000100000").unwrap(),
        "100ms"
    );

    let mut output = [0u8; 12];
    let expected = b"123456123456";
    let t = read_time12(expected).unwrap();
    assert_eq!(12 * 3600 + 34 * 60 + 56, t / NS_IN_SEC);
    assert_eq!(123456000, t % NS_IN_SEC);
    write_time12(t, &mut output);
    assert_eq!(expected, &output);
}

pub fn read_time12_native(raw: &[u8]) -> Result<u64> {
    if raw.len() != 12 {
        return Err(Error {
            _msg: "Invalid time12",
            _payload: raw,
        });
    }
    let t = read_time6(&raw[0..6])?;
    let f: u64 = fold_digits(&raw[6..12])?;
    Ok(t + f * 1000)
}

#[cfg(target_feature = "sse2")]
pub fn read_time12_vec(raw: &[u8]) -> Result<u64> {
    let mut invalid = false;
    let mut digits = [0u8; 16];
    assert!(raw.len() == 12);
    digits[0..12].copy_from_slice(raw);

    let r = unsafe { read_time12v(digits, &mut invalid) };
    if invalid {
        Err(Error {
            _msg: "Invalid time12",
            _payload: raw,
        })
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

    // Add one more multiplication to convert result to nanoseconds
    ((res >> 32) + (res & 0xFFFFFFFF) * 10000) * 1000
}
