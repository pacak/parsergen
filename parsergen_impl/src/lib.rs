use std::marker::PhantomData;

pub mod primitives;
use primitives::*;
pub mod time;

pub mod numbers;
pub use numbers::*;

pub type Result<'a, T, E = Error<'a>> = core::result::Result<T, E>;
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

pub trait Parsergen {
    const WIDTH: usize;

    fn des(raw: &[u8]) -> Result<Self>
    where
        Self: Sized;
    fn ser(&self, raw: &mut [u8]);
    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
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

#[inline(always)]
pub fn width_check<'a, const WIDTH: usize>(
    _payload: &'a [u8],
    _msg: &'static str,
) -> Result<'a, ()> {
    if _payload.len() == WIDTH {
        Ok(())
    } else {
        Err(Error { _msg, _payload })
    }
}

impl<const N: usize> std::fmt::Debug for PrettyArrBytes<'_, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.chunks(self.0.len() / N).map(PrettyBytes))
            .finish()
    }
}

pub fn parse_literal<'a, T: Default>(lit: &'static [u8], raw: &'a [u8]) -> Result<'a, T> {
    if lit == raw {
        Ok(T::default())
    } else {
        Err(Error {
            _msg: "not a valid literal",
            _payload: raw,
        })
    }
}

pub struct FixedArr<Ty, Iso, const CNT: usize>(pub [Ty; CNT], PhantomData<Iso>);
impl<Ty, Iso, const CNT: usize> From<[Ty; CNT]> for FixedArr<Ty, Iso, CNT> {
    fn from(arr: [Ty; CNT]) -> Self {
        FixedArr(arr, PhantomData::default())
    }
}

impl<Ty, Iso, const CNT: usize> Parsergen for FixedArr<Ty, Iso, CNT>
where
    Iso: Parsergen + Into<Ty>,
    Ty: Default + Copy + Into<Iso>,
    Self: Sized,
{
    const WIDTH: usize = <Iso as Parsergen>::WIDTH * CNT;

    fn des(raw: &[u8]) -> Result<Self> {
        let mut res = [Ty::default(); CNT];
        for (ix, raw) in raw.chunks(<Iso as Parsergen>::WIDTH).enumerate() {
            res[ix] = <Iso as Parsergen>::des(raw)?.into();
        }
        Ok(Self(res, PhantomData::default()))
    }

    fn ser(&self, raw: &mut [u8]) {
        for (&ty, raw_c) in self.0.iter().zip(raw.chunks_mut(<Iso as Parsergen>::WIDTH)) {
            ty.into().ser(raw_c);
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                raw.chunks(<Iso as Parsergen>::WIDTH)
                    .map(<Sliced<Iso>>::from),
            )
            .finish()
    }
}

pub struct Sliced<'a, T>(&'a [u8], PhantomData<T>);

impl<'a, T> From<&'a [u8]> for Sliced<'a, T> {
    fn from(val: &'a [u8]) -> Self {
        Self(val, PhantomData::default())
    }
}

impl<T: Parsergen> std::fmt::Debug for Sliced<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Parsergen>::slice(self.0, f)
    }
}

pub struct SlicedOwned<T>(Vec<u8>, PhantomData<T>);

impl<T> From<&[u8]> for SlicedOwned<T> {
    fn from(val: &[u8]) -> Self {
        Self(Vec::from(val), PhantomData::default())
    }
}

impl<T: Parsergen> std::fmt::Debug for SlicedOwned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Parsergen>::slice(&self.0, f)
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Default)]
pub struct Filler<const W: usize>;
impl<const W: usize> Parsergen for Filler<W> {
    const WIDTH: usize = W;

    fn des(_raw: &[u8]) -> Result<Self> {
        Ok(Self)
    }

    fn ser(&self, res: &mut [u8]) {
        for c in res.iter_mut() {
            *c = b' '
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
    }
}

impl<T: Parsergen> Parsergen for Option<T> {
    const WIDTH: usize = T::WIDTH;

    fn des(raw: &[u8]) -> Result<Self> {
        match T::des(raw) {
            Ok(ok) => Ok(Some(ok)),
            Err(_) if raw.iter().all(|sym| *sym == b' ') => Ok(None),
            Err(_) => Err(Error {
                _msg: "Not an Option<T>",
                _payload: raw,
            }),
        }
    }

    fn ser(&self, res: &mut [u8]) {
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

impl<T, const CNT: usize> Parsergen for [T; CNT]
where
    T: Parsergen + Default + Copy,
{
    const WIDTH: usize = T::WIDTH * CNT;

    #[inline(always)]
    fn des(raw: &[u8]) -> Result<Self> {
        let mut res = [T::default(); CNT];
        for (ix, raw) in raw.chunks(T::WIDTH).enumerate() {
            res[ix] = T::des(raw)?;
        }
        Ok(res)
    }

    fn ser(&self, raw: &mut [u8]) {
        for (&ty, raw_c) in self.iter().zip(raw.chunks_mut(<T as Parsergen>::WIDTH)) {
            ty.ser(raw_c);
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(raw.chunks(<T as Parsergen>::WIDTH).map(<Sliced<T>>::from))
            .finish()
    }
}

impl<const CNT: usize> Parsergen for [u8; CNT] {
    const WIDTH: usize = CNT;

    fn des(raw: &[u8]) -> Result<Self>
    where
        Self: Sized,
    {
        let mut res = [0; CNT];
        for (p, c) in res.iter_mut().zip(raw.iter()) {
            *p = *c;
        }
        Ok(res)
    }

    fn ser(&self, raw: &mut [u8]) {
        for (p, c) in raw.iter_mut().zip(self.iter()) {
            *p = *c;
        }
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
    }
}

pub fn encode<T: Parsergen>(val: &T) -> Vec<u8> {
    let mut res = vec![0; T::WIDTH];
    val.ser(&mut res);
    res
}

pub fn decode<T: Parsergen>(raw: &[u8]) -> Option<T> {
    T::des(raw).ok()
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

impl<const CNT: usize> Parsergen for Cents<CNT> {
    const WIDTH: usize = CNT;

    fn des(raw: &[u8]) -> Result<Self>
    where
        Self: Sized,
    {
        let sign = match raw[0] {
            b' ' => 1,
            b'0' => 1,
            b'-' => -1,
            _ => {
                return Err(Error {
                    _msg: "invalid sign",
                    _payload: raw,
                })
            }
        };
        if raw[CNT - 3] != b'.' {
            return Err(Error {
                _msg: "missing dot in cents",
                _payload: raw,
            });
        }

        let num1: u64 = fold_digits(&raw[1..CNT - 3])?;
        let num2: u64 = fold_digits(&raw[CNT - 2..])?;

        Ok(Cents(sign * (num1 as i64 * 100 + num2 as i64)))
    }

    fn ser(&self, raw: &mut [u8]) {
        if self.0 >= 0 {
            raw[0] = b' ';
        } else {
            raw[0] = b'-';
        }
        let num = self.0.abs();
        let num1 = num / 100;
        let num2 = num % 100;
        raw[CNT - 3] = b'.';
        unfold_digits(num1, &mut raw[1..CNT - 3]);
        unfold_digits(num2, &mut raw[CNT - 2..]);
    }
}

impl Parsergen for primitives::ISIN {
    const WIDTH: usize = 12;

    fn des(raw: &[u8]) -> Result<Self>
    where
        Self: Sized,
    {
        use std::convert::TryFrom;
        let err = Error {
            _msg: "Not an ISIN",
            _payload: raw,
        };
        match <[u8; 12]>::try_from(raw) {
            Ok(buf) => match fold_isin(buf) {
                Some(isin) => Ok(isin),
                None => Err(err),
            },
            Err(_) => Err(err),
        }
    }

    fn ser(&self, raw: &mut [u8]) {
        let buf = primitives::unfold_isin(*self);
        for (t, s) in raw.iter_mut().zip(buf.iter()) {
            *t = *s;
        }
    }
}
