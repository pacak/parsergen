/// use `parse_fixed!` instead
#[inline(always)]
#[must_use]
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
pub fn unfold_digits<T>(mut val: T, res: &mut [u8])
where
    T: std::ops::Rem<Output = T>
        + std::ops::DivAssign<T>
        + std::convert::TryInto<u8>
        + Copy
        + From<u8>,
    <T as std::convert::TryInto<u8>>::Error: std::fmt::Debug,
{
    for place in res.iter_mut().rev() {
        *place = b'0'
            + (val % 10u8.into())
                .try_into()
                .expect("any T % 10 should fit into u8");
        val /= 10.into();
    }
}

#[must_use]
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

#[derive(Copy, Clone, Debug)]
pub struct ISIN(pub u64);

/// folds a valid ISIN into u64, see [`unfold_isin`]
#[must_use]
pub fn fold_isin(raw: [u8; 12]) -> Option<ISIN> {
    let mut res: u64 = 0;

    for c in &raw {
        res *= 36;
        res += match c {
            b'0'..=b'9' => u64::from(c - b'0'),
            b'A'..=b'Z' => u64::from(c - b'A' + 10),
            _ => return None,
        }
    }
    if luhn3::valid(&raw) {
        Some(ISIN(res))
    } else {
        None
    }
}

/// folds a valid ISIN into u64, see [`unfold_isin`]
///
/// No digit check is performed
#[must_use]
pub fn fold_isin_unchecked(raw: [u8; 12]) -> Option<ISIN> {
    let mut res: u64 = 0;

    for c in &raw {
        res *= 36;
        res += match c {
            b'0'..=b'9' => u64::from(c - b'0'),
            b'A'..=b'Z' => u64::from(c - b'A' + 10),
            _ => return None,
        }
    }
    Some(ISIN(res))
}

/// Folds a valid ISIN with check digit missing into u64
///
/// check digit will be calculated and used instead of one present
/// in the array
#[must_use]
pub fn check_and_fold(mut raw: [u8; 12]) -> Option<ISIN> {
    let mut res: u64 = 0;
    raw[11] = luhn3::checksum(&raw[..11])?;

    for c in &raw {
        res *= 36;
        res += match c {
            b'0'..=b'9' => u64::from(c - b'0'),
            b'A'..=b'Z' => u64::from(c - b'A' + 10),
            _ => return None,
        }
    }
    Some(ISIN(res))
}

/// unfolds a valid ISIN into an array of ASCII bytes, see [`fold_isin`]
#[must_use]
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

#[test]
fn test_check_fold_unfold() {
    let msg_raw = b"AU0000XVGZA ";
    let msg_exp = b"AU0000XVGZA3";

    let folded = check_and_fold(*msg_raw).unwrap();
    assert_eq!(&unfold_isin(folded), msg_exp);
}
