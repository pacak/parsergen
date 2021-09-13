use std::marker::PhantomData;

pub mod primitives;
use primitives::*;

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
    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

// {{{
pub struct PrettyBytes<'a>(pub &'a [u8]);
impl std::fmt::Debug for PrettyBytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "\"")?;
        for &b in self.0 {
            if b == b'\n' {
                write!(f, "\\n")?;
            } else if b == b'\r' {
                write!(f, "\\r")?;
            } else if b == b'\t' {
                write!(f, "\\t")?;
            } else if b == b'\\' || b == b'"' {
                write!(f, "\\{}", b as char)?;
            } else if b == b'\0' {
                write!(f, "\\0")?;
            // ASCII printable
            } else if b >= 0x20 && b < 0x7f {
                write!(f, "{}", b as char)?;
            } else {
                write!(f, "\\x{:02x}", b)?;
            }
        }
        write!(f, "\"")?;
        Ok(())
    }
}

pub struct PrettyArrBytes<'a>(pub &'a [u8], pub usize);
impl std::fmt::Debug for PrettyArrBytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.chunks(self.0.len() / self.1).map(PrettyBytes))
            .finish()
    }
}
// }}}

#[derive(Copy, Clone, Default)]
pub struct FixedT<T, const WIDTH: usize>(pub T);

impl<T, const WIDTH: usize> FixedT<T, WIDTH> {
    pub fn new(val: T) -> Self {
        Self(val)
    }
}

macro_rules! derive_fixed {
    ($ty:ty, $fold:ident, $unfold:ident) => {
        impl<const WIDTH: usize> Parsergen for FixedT<$ty, WIDTH> {
            const WIDTH: usize = WIDTH;

            fn des(raw: &[u8]) -> Result<Self> {
                Ok(FixedT($fold(raw)?))
            }

            fn ser(&self, res: &mut [u8]) {
                $unfold(self.0, res);
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", PrettyBytes(raw))
            }
        }
    };
}

derive_fixed!(u8, fold_digits, unfold_digits);
derive_fixed!(u16, fold_digits, unfold_digits);
derive_fixed!(u32, fold_digits, unfold_digits);
derive_fixed!(u64, fold_digits, unfold_digits);
derive_fixed!(usize, fold_digits, unfold_digits);

//derive_fixed!(i8, fold_signed, unfold_signed);
derive_fixed!(i16, fold_signed, unfold_signed);
derive_fixed!(i32, fold_signed, unfold_signed);
derive_fixed!(i64, fold_signed, unfold_signed);
derive_fixed!(isize, fold_signed, unfold_signed);

pub struct FixedArrT<T, const WIDTH: usize, const CNT: usize>(pub [T; CNT]);

impl<T, const WIDTH: usize, const CNT: usize> FixedArrT<T, WIDTH, CNT> {
    pub fn new(val: [T; CNT]) -> Self {
        Self(val)
    }
}

impl<T, const WIDTH: usize, const CNT: usize> Parsergen for FixedArrT<T, WIDTH, CNT>
where
    T: Default + Copy,
    FixedT<T, WIDTH>: Parsergen,
{
    const WIDTH: usize = WIDTH * CNT;

    fn des(raw: &[u8]) -> Result<Self> {
        let mut res = [T::default(); CNT];
        for (ix, raw) in raw.chunks(WIDTH).enumerate() {
            res[ix] = FixedT::des(raw)?.0;
        }
        Ok(Self(res))
    }

    fn ser(&self, raw: &mut [u8]) {
        for (val, chunk) in self.0.iter().zip(raw.chunks_mut(WIDTH)) {
            <FixedT<T, WIDTH> as Parsergen>::ser(&FixedT::new(*val), chunk);
        }
    }

    fn slice(_raw: &[u8], _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Filler<const W: usize>;
impl<const W: usize> Parsergen for Filler<W> {
    const WIDTH: usize = W;

    fn des<'a>(_raw: &'a [u8]) -> Result<Self> {
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

    fn des<'a>(raw: &'a [u8]) -> Result<Self> {
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

    fn des<'a>(raw: &'a [u8]) -> Result<Self> {
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
