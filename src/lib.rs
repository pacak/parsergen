use std::array::TryFromSliceError;
use std::marker::PhantomData;

extern crate self as parsergen;

pub mod numbers;
pub mod primitives;
pub mod time;
pub use crate::numbers::*;

use crate::primitives::{fold_digits, fold_isin, unfold_digits};
pub use arrayref;
pub use parsergen_derive::*;

pub trait HasWidth {
    const WIDTH: usize;
}

pub struct ValidBytes<'a, T, const W: usize> {
    raw: &'a [u8],
    typ: PhantomData<T>,
}

pub struct Signed;
pub struct Unsigned;

impl<'a, T: Parsergen<W>, const W: usize> std::fmt::Debug for ValidBytes<'a, T, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match T::decode(self.raw) {
            Ok(Some(_)) => write!(f, "{:?}", PrettyBytes(self.raw)),
            Ok(None) => write!(f, "\x1b[31m{:?}\x1b[30;0m", PrettyBytes(self.raw)),
            Err(_) => write!(f, "\x1b[35m{:?}\x1b[30;0m", PrettyBytes(self.raw)),
        }
    }
}

impl<'a, const W: usize> std::fmt::Debug for ValidBytes<'a, Unsigned, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let valid = self.raw.iter().all(|&c| (b'0'..=b'9').contains(&c));
        if valid {
            write!(f, "{:?}", PrettyBytes(self.raw))
        } else {
            write!(f, "\x1b[31m{:?}\x1b[30;0m", PrettyBytes(self.raw))
        }
    }
}

impl<'a, const W: usize> std::fmt::Debug for ValidBytes<'a, Signed, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let first_valid = !self.raw.is_empty()
            && (self.raw[0] == b' ' || self.raw[0] == b'-' || self.raw[0] == b'0');
        let tail_valid = self.raw.iter().skip(1).all(|&c| (b'0'..=b'9').contains(&c));
        if first_valid && tail_valid {
            write!(f, "{:?}", PrettyBytes(self.raw))
        } else {
            write!(f, "\x1b[31m{:?}\x1b[30;0m", PrettyBytes(self.raw))
        }
    }
}

impl<'a, T, const W: usize> From<&'a [u8]> for ValidBytes<'a, T, W> {
    fn from(raw: &'a [u8]) -> Self {
        Self {
            raw,
            typ: PhantomData,
        }
    }
}

pub trait Parsergen<const W: usize>
where
    Self: HasWidth,
{
    /// Parse a thing from fixed width array
    fn des(raw: &[u8; W]) -> Option<Self>
    where
        Self: Sized;

    /// store a thing into a fixed width array
    fn ser(&self, raw: &mut [u8; W]);

    /// split a slice into array and annotate it with how it would be parsed  instead of actually
    /// parsing it
    ///
    /// # Errors
    ///
    /// uses `write!` internally
    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    where
        Self: Sized,
    {
        let typ = PhantomData;
        write!(f, "{:?}", ValidBytes::<Self, W> { typ, raw })
    }

    /// parse thing from a slice
    ///
    /// Err indicates invalid length, None indicates invalid content
    ///
    /// # Errors
    ///
    /// Fails with `TryFromSliceError` if input length is not `W`
    fn decode(raw: &[u8]) -> Result<Option<Self>, TryFromSliceError>
    where
        Self: Sized,
    {
        raw.try_into().map(|arr| Self::des(arr))
    }
}

/// Pretty print a slice of bytes, non ASCII are escaped in a way similar to
/// [`escape_default`][::std::primitive::char::escape_default] but with hex
/// escape instead of unicode one
pub struct PrettyBytes<'a>(pub &'a [u8]);
impl std::fmt::Debug for PrettyBytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "\"")?;
        for &b in self.0 {
            match b {
                0 => write!(f, "\\0")?,
                b'\n' => write!(f, "\\n")?,
                b'\r' => write!(f, "\\r")?,
                b'\t' => write!(f, "\\t")?,
                b'"' => write!(f, "\"")?,
                b'\\' => write!(f, "\\\\")?,
                // ASCII printable
                0x20..=0x7f => write!(f, "{}", b as char)?,
                _ => write!(f, "\\x{:02x}", b)?,
            }
        }
        write!(f, "\"")?;
        Ok(())
    }
}

impl<'a> From<&'a [u8]> for PrettyBytes<'a> {
    fn from(xs: &'a [u8]) -> Self {
        PrettyBytes(xs)
    }
}

/// Pretty print a slice of bytes as an array of chunks
pub struct PrettyArrBytes<'a, const N: usize>(pub &'a [u8]);

impl<'a, const N: usize> PrettyArrBytes<'a, N> {
    #[must_use]
    pub const fn new(val: &'a [u8]) -> Self {
        Self(val)
    }
}

impl<const N: usize> std::fmt::Debug for PrettyArrBytes<'_, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.chunks(self.0.len() / N).map(PrettyBytes))
            .finish()
    }
}

/// parse a static pattern into a default value
///
/// used internally
pub fn parse_literal<T: Default>(lit: &'static [u8], raw: &[u8]) -> Option<T> {
    (lit == raw).then(T::default)
}

/// used for sliced printing of unparsed messages
pub struct Sliced<'a, T, const WIDTH: usize>(&'a [u8], PhantomData<T>);

impl<'a, T, const WIDTH: usize> From<&'a [u8]> for Sliced<'a, T, WIDTH> {
    fn from(val: &'a [u8]) -> Self {
        Self(val, PhantomData)
    }
}

/// used internally for sliced printing of unparsed messages
pub struct SlicedArr<'a, T, const WIDTH: usize>(&'a [u8], PhantomData<T>);
impl<'a, T, const WIDTH: usize> From<&'a [u8]> for SlicedArr<'a, T, WIDTH> {
    fn from(val: &'a [u8]) -> Self {
        Self(val, PhantomData)
    }
}

impl<T: Parsergen<WIDTH>, const WIDTH: usize> std::fmt::Debug for SlicedArr<'_, T, WIDTH> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.chunks(WIDTH).map(Sliced::<T, WIDTH>::from))
            .finish()
    }
}

impl<T: Parsergen<WIDTH>, const WIDTH: usize> std::fmt::Debug for Sliced<'_, T, WIDTH> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Parsergen<WIDTH>>::slice(self.0, f)
    }
}

/// used for sliced printing of unparsed messages
pub struct SlicedOwned<T, const WIDTH: usize>(Vec<u8>, PhantomData<T>);

impl<T, const WIDTH: usize> From<&[u8]> for SlicedOwned<T, WIDTH> {
    fn from(val: &[u8]) -> Self {
        Self(Vec::from(val), PhantomData::default())
    }
}

impl<T: Parsergen<WIDTH>, const WIDTH: usize> std::fmt::Debug for SlicedOwned<T, WIDTH> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Parsergen<WIDTH>>::slice(&self.0, f)
    }
}

/// consume and return an item without any parsing
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Blob<const WIDTH: usize>(pub [u8; WIDTH]);
impl<const WIDTH: usize> HasWidth for Blob<WIDTH> {
    const WIDTH: usize = WIDTH;
}

impl<const WIDTH: usize> Parsergen<WIDTH> for Blob<WIDTH> {
    fn des(raw: &[u8; WIDTH]) -> Option<Self> {
        Some(Self(*raw))
    }

    fn ser(&self, res: &mut [u8; WIDTH]) {
        *res = self.0;
    }
}

/// consume an item without any parsing, fill back with spaces
#[derive(Eq, PartialEq, Copy, Clone, Debug, Default)]
pub struct Filler<const WIDTH: usize>;
impl<const WIDTH: usize> HasWidth for Filler<WIDTH> {
    const WIDTH: usize = WIDTH;
}
impl<const WIDTH: usize> Parsergen<WIDTH> for Filler<WIDTH> {
    fn des(_raw: &[u8; WIDTH]) -> Option<Self> {
        Some(Self)
    }

    fn ser(&self, res: &mut [u8; WIDTH]) {
        for c in res.iter_mut() {
            *c = b' ';
        }
    }
}

impl<T: HasWidth> HasWidth for Option<T> {
    const WIDTH: usize = T::WIDTH;
}

impl<T: Parsergen<WIDTH> + HasWidth, const WIDTH: usize> Parsergen<WIDTH> for Option<T> {
    fn des(raw: &[u8; WIDTH]) -> Option<Self> {
        match T::des(raw) {
            Some(ok) => Some(Some(ok)),
            None if raw.iter().all(|sym| *sym == b' ') => Some(None),
            None => None,
        }
    }

    fn ser(&self, res: &mut [u8; WIDTH]) {
        match self {
            Some(nested) => nested.ser(res),
            None => {
                for c in res.iter_mut() {
                    *c = b' ';
                }
            }
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if raw.iter().all(|c| *c == b' ') {
            write!(f, "{:?}", PrettyBytes(raw))
        } else {
            T::slice(raw, f)
        }
    }
}

/// used internally to parse arrays of of items
///
/// # Panics
///
/// Panics if invariant of `WIDTH == FWIDTH * CNT` is not maintained. This invariant is enforced
/// by proc macros
#[must_use]
pub fn des_array<
    T: Parsergen<FWIDTH> + Default + Copy,
    const WIDTH: usize,
    const FWIDTH: usize,
    const CNT: usize,
>(
    raw: &[u8; WIDTH],
) -> Option<[T; CNT]> {
    let mut res: [T; CNT] = [T::default(); CNT];

    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks(FWIDTH).enumerate() {
        let buf = <[u8; FWIDTH]>::try_from(chunk).expect("Function is misused");
        res[ix] = T::des(&buf)?;
    }
    Some(res)
}

/// Used internally to parse arrays of of items
///
/// # Panics
///
/// Panics if invariant of `WIDTH == FWIDTH * CNT` is not maintained. This invariant is enforced
/// by proc macros
#[must_use]
pub fn des_iso_array<T, Iso, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    raw: &[u8; WIDTH],
) -> Option<[T; CNT]>
where
    Iso: Parsergen<FWIDTH>,
    T: Default + Copy + From<Iso>,
{
    let mut res = [T::default(); CNT];

    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks(FWIDTH).enumerate() {
        let buf = <[u8; FWIDTH]>::try_from(chunk).expect("Functions is misused");
        res[ix] = T::try_from(Iso::des(&buf)?).ok()?;
    }

    Some(res)
}

/// used internally to serialize arrays of of items
///
/// # Panics
///
/// Panics if invariant of `WIDTH == FWIDTH * CNT` is not maintained. This invariant is enforced
/// by proc macros
pub fn ser_array<
    T: Parsergen<FWIDTH> + Default + Copy,
    const WIDTH: usize,
    const FWIDTH: usize,
    const CNT: usize,
>(
    arr: [T; CNT],
    raw: &mut [u8; WIDTH],
) {
    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks_mut(FWIDTH).enumerate() {
        let buf = <&mut [u8; FWIDTH]>::try_from(chunk).expect("Function is misused");
        arr[ix].ser(buf);
    }
}

/// used internally to serialize arrays of of items
///
/// # Panics
///
/// Panics if invariant of `WIDTH == FWIDTH * CNT` is not maintained. This invariant is enforced
/// by proc macros
pub fn ser_iso_array<T, Iso, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    arr: [T; CNT],
    raw: &mut [u8; WIDTH],
) where
    T: Default + Copy,
    Iso: Parsergen<FWIDTH> + From<T>,
{
    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks_mut(FWIDTH).enumerate() {
        let buf = <&mut [u8; FWIDTH]>::try_from(chunk).expect("Function is misused");

        Iso::from(arr[ix]).ser(buf);
    }
}

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

impl HasWidth for primitives::ISIN {
    const WIDTH: usize = 12;
}
impl Parsergen<12> for primitives::ISIN {
    fn des(raw: &[u8; 12]) -> Option<Self> {
        fold_isin(*raw)
    }

    fn ser(&self, raw: &mut [u8; 12]) {
        *raw = primitives::unfold_isin(*self);
    }
}
