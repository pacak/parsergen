use std::marker::PhantomData;

pub mod primitives;
use primitives::*;
pub mod time;

pub mod numbers;
pub use numbers::*;

extern crate self as parsergen;

use parsergen_derive::*;
#[derive(Parsergen)]
struct Foo {
    foo: u32,
}

#[derive(Debug)]
pub struct Error<'a> {
    pub _msg: &'static str,
    pub _payload: &'a [u8],
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parse error: {}, payload: {:?}",
            self._msg,
            PrettyBytes(self._payload)
        )
    }
}

impl<'a> std::error::Error for Error<'a> {}

pub trait HasWidth {
    const WIDTH: usize;
}

pub trait Parsergen<const W: usize>
where
    Self: HasWidth,
{
    fn des(raw: &[u8; W]) -> Option<Self>
    where
        Self: Sized;
    fn ser(&self, raw: &mut [u8; W]);
    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
    }

    fn decode(raw: &[u8]) -> Option<Self>
    where
        Self: Sized,
    {
        match raw.try_into() {
            Ok(arr) => Self::des(arr),
            Err(_) => None,
        }
    }
}

//
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

pub struct PrettyArrBytes<'a, const N: usize>(pub &'a [u8]);

impl<'a, const N: usize> PrettyArrBytes<'a, N> {
    pub fn new(val: &'a [u8]) -> Self {
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

pub fn parse_literal<'a, T: Default>(lit: &'static [u8], raw: &'a [u8]) -> Option<T> {
    if lit == raw {
        Some(T::default())
    } else {
        None
    }
}

pub struct FixedArr<Ty, Iso, const CNT: usize>(pub [Ty; CNT], PhantomData<Iso>);
impl<Ty, Iso, const CNT: usize> From<[Ty; CNT]> for FixedArr<Ty, Iso, CNT> {
    fn from(arr: [Ty; CNT]) -> Self {
        FixedArr(arr, PhantomData)
    }
}

pub struct Sliced<'a, T, const WIDTH: usize>(&'a [u8], PhantomData<T>);

impl<'a, T, const WIDTH: usize> From<&'a [u8]> for Sliced<'a, T, WIDTH> {
    fn from(val: &'a [u8]) -> Self {
        Self(val, PhantomData)
    }
}
/*
impl<T, const FWIDTH: usize, const WIDTH: usize, const CNT: usize> Parsergen<WIDTH> for [T; CNT]
where
    T: Parsergen<FWIDTH> + Default + Copy,
{
    fn des(raw: &[u8; WIDTH]) -> Option<Self> {
        let mut res = [T::default(); CNT];
        for (ix, raw) in raw.chunks(FWIDTH).enumerate() {
            res[ix] = T::des(raw)?;
        }
        Ok(res)
    }

    fn ser(&self, raw: &mut [u8; WIDTH]) {
        for (&ty, raw_c) in self.iter().zip(raw.chunks_mut(FWIDTH)) {
            ty.ser(raw_c);
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(raw.chunks(FWIDTH).map(<Sliced<T>>::from))
            .finish()
    }
}*/

impl<T: Parsergen<WIDTH>, const WIDTH: usize> std::fmt::Debug for Sliced<'_, T, WIDTH> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Parsergen<WIDTH>>::slice(self.0, f)
    }
}

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

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
    }
}

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
            *c = b' '
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
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
                    *c = b' '
                }
            }
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        T::slice(raw, f)
    }
}

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
        let buf = <[u8; FWIDTH]>::try_from(chunk).unwrap();
        res[ix] = T::des(&buf)?;
    }
    Some(res)
}

pub fn des_iso_array<T, Iso, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    raw: &[u8; WIDTH],
) -> Option<[T; CNT]>
where
    Iso: Parsergen<FWIDTH>,
    T: Default + Copy,
    T: From<Iso>,
{
    let mut res = [T::default(); CNT];

    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks(FWIDTH).enumerate() {
        let buf = <[u8; FWIDTH]>::try_from(chunk).unwrap();
        res[ix] = T::from(Iso::des(&buf)?);
    }

    Some(res)
}

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
        let buf = <&mut [u8; FWIDTH]>::try_from(chunk).unwrap();
        arr[ix].ser(buf)
    }
}

pub fn ser_iso_array<T, Iso, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    arr: [T; CNT],
    raw: &mut [u8; WIDTH],
) where
    T: Default + Copy,
    Iso: Parsergen<FWIDTH> + From<T>,
{
    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks_mut(FWIDTH).enumerate() {
        let buf = <&mut [u8; FWIDTH]>::try_from(chunk).unwrap();

        Iso::from(arr[ix]).ser(buf)
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
            b' ' => 1,
            b'0' => 1,
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
            raw[0] = b' ';
        } else {
            raw[0] = b'-';
        }
        let num = self.0.abs();
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
    fn des(raw: &[u8; 12]) -> Option<Self>
    where
        Self: Sized,
    {
        fold_isin(*raw)
    }

    fn ser(&self, raw: &mut [u8; 12]) {
        *raw = primitives::unfold_isin(*self);
    }
}

pub fn slice_arr<T: Parsergen<W>, const W: usize>(
    raw: &[u8],
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let mut d = f.debug_list();
    d.entries(raw.chunks(W).map(|c| Sliced::<T, W>::from(c)));
    d.finish()
}
