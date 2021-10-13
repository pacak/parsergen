use std::marker::PhantomData;

pub mod primitives;
use primitives::*;
pub mod time;

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
/////////////////////////////////////////////////////////////////////////////////////////////////
pub trait Decode {
    const WIDTH: usize;

    fn decode(raw: &[u8]) -> Result<Self>
    where
        Self: Sized;
}

/////////////////////////////////////////////////////////////////////////////////////////////////
pub trait Encode {
    fn encode(&self, raw: &mut [u8]);
}

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

impl<const N: usize> std::fmt::Debug for PrettyArrBytes<'_, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.chunks(self.0.len() / N).map(PrettyBytes))
            .finish()
    }
}
//

#[derive(Copy, Clone, Default)]
pub struct FixedT<T, const WIDTH: usize>(pub T);

impl<T, const WIDTH: usize> FixedT<T, WIDTH> {
    pub fn new(val: T) -> Self {
        Self(val)
    }
}

macro_rules! derive_fixed {
    ($ty:ty, $fold:ident, $unfold:ident) => {
        impl<const WIDTH: usize> Decode for FixedT<$ty, WIDTH> {
            const WIDTH: usize = WIDTH;
            #[inline]
            fn decode(raw: &[u8]) -> Result<Self> {
                if raw.len() == WIDTH {
                    Ok(FixedT($fold(raw)?))
                } else {
                    Err(Error {
                        _msg: "invalid width",
                        _payload: raw,
                    })
                }
            }
        }

        impl<const WIDTH: usize> Parsergen for FixedT<$ty, WIDTH> {
            const WIDTH: usize = WIDTH;

            #[inline]
            fn des(raw: &[u8]) -> Result<Self> {
                if raw.len() == WIDTH {
                    Ok(FixedT($fold(raw)?))
                } else {
                    Err(Error {
                        _msg: "invalid width",
                        _payload: raw,
                    })
                }
            }

            fn ser(&self, res: &mut [u8]) {
                $unfold(self.0, res);
            }
        }
    };
}

impl<const WIDTH: usize> Parsergen for FixedT<i8, WIDTH> {
    const WIDTH: usize = WIDTH;
    #[inline]
    fn des(raw: &[u8]) -> Result<Self> {
        let err = Error {
            _msg: "invalid width",
            _payload: raw,
        };

        use std::convert::TryFrom;
        if raw.len() == WIDTH {
            let x16: i16 = fold_signed(raw)?;
            match i8::try_from(x16) {
                Ok(i) => Ok(FixedT(i)),
                _ => Err(err),
            }
        } else {
            Err(Error {
                _msg: "invalid width",
                _payload: raw,
            })
        }
    }

    fn ser(&self, res: &mut [u8]) {
        unfold_signed(self.0, res);
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
    }
}

derive_fixed!(u8, fold_digits, unfold_digits);
derive_fixed!(u16, fold_digits, unfold_digits);
derive_fixed!(u32, fold_digits, unfold_digits);
derive_fixed!(u64, fold_digits, unfold_digits);
derive_fixed!(usize, fold_digits, unfold_digits);

derive_fixed!(i16, fold_signed, unfold_signed);
derive_fixed!(i32, fold_signed, unfold_signed);
derive_fixed!(i64, fold_signed, unfold_signed);
derive_fixed!(isize, fold_signed, unfold_signed);

#[derive(Debug)]
pub struct FixedArrT<T, const WIDTH: usize, const CNT: usize>(pub [T; CNT]);

impl<T, const WIDTH: usize, const CNT: usize> FixedArrT<T, WIDTH, CNT> {
    pub fn new(val: [T; CNT]) -> Self {
        Self(val)
    }
}

impl<T, const WIDTH: usize, const CNT: usize> Parsergen for FixedArrT<T, WIDTH, CNT>
where
    T: Default + Copy + std::fmt::Debug,
    FixedT<T, WIDTH>: Parsergen,
{
    const WIDTH: usize = WIDTH * CNT;

    #[inline]
    fn des(raw: &[u8]) -> Result<Self> {
        let mut res = [T::default(); CNT];
        for (ix, raw) in raw.chunks(WIDTH).enumerate() {
            res[ix] = FixedT::des(raw)?.0;
        }
        Ok(Self(res))
    }

    fn ser(&self, raw: &mut [u8]) {
        println!(
            "Will try to write {:?} to {:?}",
            self,
            raw.chunks_mut(WIDTH)
        );
        for (val, chunk) in self.0.iter().zip(raw.chunks_mut(WIDTH)) {
            println!("writing {:?} to {:?}", val, chunk);
            <FixedT<T, WIDTH> as Parsergen>::ser(&FixedT::new(*val), chunk);
        }
    }

    fn slice(_raw: &[u8], _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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

impl<'a, T, const W: usize> From<&'a [u8; W]> for Sliced<'a, T> {
    fn from(val: &'a [u8; W]) -> Self {
        Sliced(val, PhantomData::default())
    }
}

impl<'a, T> From<&'a [u8]> for Sliced<'a, T> {
    fn from(val: &'a [u8]) -> Self {
        Sliced(val, PhantomData::default())
    }
}

impl<T: Parsergen> std::fmt::Debug for Sliced<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Parsergen>::slice(self.0, f)
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
