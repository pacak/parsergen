use crate::primitives::unfold_digits;

const NS_IN_SEC: u64 = 1_000_000_000;

/// Parse time in `HHMMSS` format
///
/// Parses time given as `HHMMSS` into nanoseconds since the beginning of the day
/// `HHMMSS` means 6 digits, no extra symbols. There's no additional checks on
/// time validity: `read_time6` will accept `25:99:99`.
/// # Examples
/// ```rust
/// use parsergen::time::*;
/// let input = b"000001";
/// let r = read_time6(input).unwrap();
/// assert_eq!(r, 1_000_000_000);
/// ```
#[must_use]
pub fn read_time6(raw: &[u8; 6]) -> Option<u64> {
    const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
    const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
    const MASK: u64 = u64::from_le_bytes([0x80; 8]);

    let mut buf = [b'0'; 8];
    buf[..6].copy_from_slice(raw);

    let v = u64::from_le_bytes(buf);
    let a = v.wrapping_add(ADD);
    let v = v.wrapping_sub(SUB);
    if (a | v) & MASK == 0 {
        let v = v << 16i32;
        let v = v.wrapping_mul(0x0A01) >> 8i32;
        let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(0x3C0001) >> 16i32;
        let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(0xE1000000001) >> 32i32;
        Some(v * NS_IN_SEC)
    } else {
        None
    }
}

#[test]
fn test_read_time6() {
    assert_eq!(read_time6(b"000001"), Some(NS_IN_SEC));
    assert_eq!(read_time6(b"000101"), Some(NS_IN_SEC * 61));
    assert_eq!(read_time6(b"010101"), Some(NS_IN_SEC * 3661));
    assert_eq!(read_time6(b"\000000"), None);
    assert_eq!(read_time6(b"xxxxx0"), None);
}

/// Parse time in `HHMMSSss` format
///
/// Parses time given as `HHMMSSss` into nanoseconds since the beginning of the day
/// `HHMMSSss` means 8 digits, no extra symbols. There's no additional checks on
/// time validity: `read_time8` will accept `25:99:99.22`.
/// # Examples
/// ```rust
/// use parsergen::time::*;
/// let input = b"000001";
/// let r = read_time6(input).unwrap();
/// assert_eq!(r, 1_000_000_000);
/// ```
#[must_use]
pub const fn read_time8(raw: &[u8; 8]) -> Option<u64> {
    const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
    const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
    const MASK: u64 = u64::from_le_bytes([0x80; 8]);

    let v = u64::from_le_bytes(*raw);
    let a = v.wrapping_add(ADD);
    let v = v.wrapping_sub(SUB);
    if (a | v) & MASK == 0 {
        let v = v.wrapping_mul(0x0A01) >> 8i32;
        let frac = (v >> 48i32) & 0xff;
        let v = v << 16i32;
        let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(0x3C0001) >> 16i32;
        let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(0xE1000000001) >> 32i32;
        Some(v * NS_IN_SEC + frac * NS_IN_SEC / 100)
    } else {
        None
    }
}

#[test]
fn test_read_time8() {
    assert_eq!(read_time8(b"00000100"), Some(NS_IN_SEC));
    assert_eq!(read_time8(b"00010100"), Some(NS_IN_SEC * 61));
    assert_eq!(read_time8(b"01010100"), Some(NS_IN_SEC * 3661));
    assert_eq!(read_time8(b"\00000000"), None);
    assert_eq!(read_time8(b"xxxxx000"), None);
    assert_eq!(read_time8(b"00000012"), Some(NS_IN_SEC / 100 * 12));
}

/// Parse time in `HHMMSSuuuuuu` format
///
/// Parses time given as `HHMMSSuuuuuu` into nanoseconds since the beginning of the day
/// `HHMMSSuuuuuu` means 12 digits, no extra symbols. There's no additional checks on
/// time validity: `read_time12` will accept `25:99:99.123456`.
/// # Examples
/// ```rust
/// use parsergen::time::*;
/// let input = b"000001000000";
/// let r = read_time12(input).unwrap();
/// assert_eq!(r, 1_000_000_000);
/// ```
#[must_use]
pub fn read_time12(raw: &[u8; 12]) -> Option<u64> {
    const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
    const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
    const MASK: u64 = u64::from_le_bytes([0x80; 8]);
    let hi;
    let lo;
    {
        let raw: [u8; 8] = raw[0..8].try_into().expect("");
        let v = u64::from_le_bytes(raw);
        let a = v.wrapping_add(ADD);
        let v = v.wrapping_sub(SUB);
        if (a | v) & MASK == 0 {
            let v = v << 16i32;
            let v = v.wrapping_mul(0x0A01) >> 8i32;
            let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(0x3C0001) >> 16i32;
            let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(0xE1000000001) >> 32i32;
            hi = v;
        } else {
            return None;
        }
    }
    {
        let raw: [u8; 8] = raw[4..12].try_into().expect("12 - 4 = 8");
        let v = u64::from_le_bytes(raw);
        let a = v.wrapping_add(ADD);
        let v = v.wrapping_sub(SUB);
        if (a | v) & MASK == 0 {
            let v = v >> 16i32 << 16i32;
            let v = v.wrapping_mul(0x0A01) >> 8i32;
            let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(0x640001) >> 16i32;
            let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(0x271000000001) >> 32i32;
            lo = v;
        } else {
            return None;
        }
    }
    Some(hi * NS_IN_SEC + lo * NS_IN_SEC / 1_000_000)
}

#[test]
fn test_read_time12() {
    assert_eq!(read_time12(b"000001000000"), Some(NS_IN_SEC));
    assert_eq!(read_time12(b"000101000000"), Some(NS_IN_SEC * 61));
    assert_eq!(read_time12(b"010101000000"), Some(NS_IN_SEC * 3661));
    assert_eq!(read_time12(b"\000000000000"), None);
    assert_eq!(read_time12(b"xxxxx0000000"), None);
    assert_eq!(read_time12(b"000000120000"), Some(NS_IN_SEC / 100 * 12));
}

/// Write time given in microseconds into `HHMMSSuu` format,
pub fn write_time6(time: u64, res: &mut [u8; 6]) {
    let seconds = time / NS_IN_SEC;
    let h = seconds / 3600;
    let m = (seconds / 60) % 60;
    let s = seconds % 60;
    unfold_digits(h, &mut res[0..2]);
    unfold_digits(m, &mut res[2..4]);
    unfold_digits(s, &mut res[4..6]);
}

/// Write time given in microseconds into `HHMMSSuu` format,
pub fn write_time8(time: u64, res: &mut [u8; 8]) {
    let seconds = time / NS_IN_SEC;
    let n = time % NS_IN_SEC * 100 / NS_IN_SEC;
    let h = seconds / 3600;
    let m = (seconds / 60) % 60;
    let s = seconds % 60;
    unfold_digits(h, &mut res[0..2]);
    unfold_digits(m, &mut res[2..4]);
    unfold_digits(s, &mut res[4..6]);
    unfold_digits(n, &mut res[6..8]);
}

/// Write time given in microseconds into `HHMMSSuuuuuu` format,
pub fn write_time12(time: u64, res: &mut [u8; 12]) {
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
    assert_eq!(12 * NS_IN_SEC / 100, t % NS_IN_SEC);
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
    //  let expected = b"123456123456";
    let expected = b"123456000000";
    let t = read_time12(expected).unwrap();
    assert_eq!(12 * 3600 + 34 * 60 + 56, t / NS_IN_SEC);
    assert_eq!(0, t % NS_IN_SEC);
    write_time12(t, &mut output);
    assert_eq!(expected, &output);
}
