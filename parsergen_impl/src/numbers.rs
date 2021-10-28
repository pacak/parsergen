use crate::primitives::*;
use crate::*;

#[derive(Copy, Clone, Default)]
/// Implements Parsergen for fixed width numbers
pub struct FixedT<T, const WIDTH: usize>(pub T);
impl<T, const WIDTH: usize> HasWidth for FixedT<T, WIDTH> {
    const WIDTH: usize = WIDTH;
}
impl<T, const WIDTH: usize> FixedT<T, WIDTH> {
    pub fn new(val: T) -> Self {
        Self(val)
    }
}

#[derive(Copy, Clone, Default)]
/// Implements Parsergen for fixed width numbers
///
/// validity check is optimized with vector instructions
pub struct FixedTV<T, const WIDTH: usize>(pub T);
impl<T, const WIDTH: usize> HasWidth for FixedTV<T, WIDTH> {
    const WIDTH: usize = WIDTH;
}
impl<T, const WIDTH: usize> FixedTV<T, WIDTH> {
    pub fn new(val: T) -> Self {
        Self(val)
    }
}

pub fn des_fixed_array<T, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    raw: &[u8; WIDTH],
) -> Result<[T; CNT]>
where
    FixedT<T, FWIDTH>: Parsergen<FWIDTH>,
    T: Default + Copy,
{
    assert_eq!(WIDTH, FWIDTH * CNT);
    let mut res = [T::default(); CNT];
    for (ix, chunk) in raw.chunks(FWIDTH).enumerate() {
        let buf = <[u8; FWIDTH]>::try_from(chunk).unwrap();
        match FixedT::<T, FWIDTH>::des(&buf) {
            Ok(ok) => res[ix] = ok.0,
            Err(err) => {
                return Err(Error {
                    _payload: raw, // TODO - use proper slice reference
                    ..err
                });
            }
        }
    }
    Ok(res)
}

pub fn ser_fixed_array<T, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    arr: [T; CNT],
    raw: &mut [u8; WIDTH],
) where
    FixedT<T, FWIDTH>: Parsergen<FWIDTH>,
    T: Copy,
{
    assert_eq!(WIDTH, FWIDTH * CNT);
    for (ix, chunk) in raw.chunks_mut(FWIDTH).enumerate() {
        let buf = <&mut [u8; FWIDTH]>::try_from(chunk).unwrap();
        FixedT::new(arr[ix]).ser(buf)
    }
}

pub fn des_fixed_array_vec<T, const WIDTH: usize, const FWIDTH: usize, const CNT: usize>(
    raw: &[u8; WIDTH],
) -> Result<[T; CNT]>
where
    T: Default + Copy,
    FixedTV<T, FWIDTH>: Parsergen<FWIDTH>,
{
    assert_eq!(WIDTH, FWIDTH * CNT);
    let mut res = [T::default(); CNT];
    for (ix, chunk) in raw.chunks(FWIDTH).enumerate() {
        let buf = <[u8; FWIDTH]>::try_from(chunk).unwrap();
        match FixedTV::<T, FWIDTH>::des(&buf) {
            Ok(ok) => res[ix] = ok.0,
            Err(err) => {
                return Err(Error {
                    _payload: raw, // TODO - use proper slice reference
                    ..err
                });
            }
        }
    }
    Ok(res)
}

macro_rules! derive_unsigned {
    ($ty:ty) => {
        impl<const WIDTH: usize> Parsergen<WIDTH> for FixedT<$ty, WIDTH> {
            #[inline(always)]
            fn des(raw: &[u8; WIDTH]) -> Result<Self> {
                width_check::<WIDTH>(raw, "invalid width for FixedT")?;

                Ok(FixedT(fold_digits(raw)?))
            }

            fn ser(&self, res: &mut [u8; WIDTH]) {
                unfold_digits(self.0, res);
            }
        }

        impl<const WIDTH: usize> Parsergen<WIDTH> for FixedTV<$ty, WIDTH> {
            #[inline(always)]
            fn des(raw: &[u8; WIDTH]) -> Result<Self> {
                width_check::<WIDTH>(raw, "invalid width for FixedT")?;
                Ok(Self(fold_digits_vec::<$ty>(raw)?))
            }

            fn ser(&self, res: &mut [u8; WIDTH]) {
                unfold_digits(self.0, res);
            }
        }
    };
}

macro_rules! derive_signed {
    ($ty:ty, $ti:ty) => {
        impl<const WIDTH: usize> Parsergen<WIDTH> for FixedT<$ty, WIDTH> {
            #[inline(always)]
            fn des(raw: &[u8; WIDTH]) -> Result<Self> {
                width_check::<WIDTH>(raw, "invalid width for FixedT")?;
                let acc: $ti = fold_digits(&raw[1..])?;
                match raw[0] {
                    b'0' | b' ' | b'+' => Ok(Self(acc as $ty)),
                    b'-' => Ok(Self(-(acc as $ty))),
                    _ => Err(Error {
                        _msg: "invalid digits (sign)",
                        _payload: raw,
                    }),
                }
            }

            fn ser(&self, res: &mut [u8; WIDTH]) {
                let val = self.0;
                res[0] = if val >= 0 { b' ' } else { b'-' };
                unfold_digits(val.abs() as $ti, &mut res[1..]);
            }
        }

        impl<const WIDTH: usize> Parsergen<WIDTH> for FixedTV<$ty, WIDTH> {
            #[inline]
            fn des(raw: &[u8; WIDTH]) -> Result<Self> {
                width_check::<WIDTH>(raw, "invalid width for FixedT")?;
                let acc: $ti = fold_digits_vec::<$ti>(&raw[1..])?;
                match raw[0] {
                    b'0' | b' ' | b'+' => Ok(Self(acc as $ty)),
                    b'-' => Ok(Self(-(acc as $ty))),
                    _ => Err(Error {
                        _msg: "invalid digits (sign)",
                        _payload: raw,
                    }),
                }
            }

            fn ser(&self, res: &mut [u8; WIDTH]) {
                let val = self.0;
                res[0] = if val >= 0 { b' ' } else { b'-' };
                unfold_digits(val.abs() as $ti, &mut res[1..]);
            }
        }
    };
}

derive_unsigned!(u8);
derive_unsigned!(u16);
derive_unsigned!(u32);
derive_unsigned!(u64);
derive_unsigned!(u128);
derive_unsigned!(usize);

derive_signed!(i8, u8);
derive_signed!(i16, u16);
derive_signed!(i32, u32);
derive_signed!(i64, u64);
derive_signed!(i128, u128);
derive_signed!(isize, usize);
/*
#[derive(Debug)]
pub struct FixedArrT<T, const WIDTH: usize, const CNT: usize>(pub [T; CNT]);

impl<T, const WIDTH: usize, const CNT: usize> FixedArrT<T, WIDTH, CNT> {
    pub fn new(val: [T; CNT]) -> Self {
        Self(val)
    }
}

pub struct FixedArrTV<T, const WIDTH: usize, const CNT: usize>(pub [T; CNT]);
impl<T, const WIDTH: usize, const CNT: usize> FixedArrTV<T, WIDTH, CNT> {
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

    #[inline(always)]
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

impl<T, const WIDTH: usize, const CNT: usize> Parsergen for FixedArrTV<T, WIDTH, CNT>
where
    T: Default + Copy + std::fmt::Debug,
    FixedTV<T, WIDTH>: Parsergen,
{
    const WIDTH: usize = WIDTH * CNT;

    #[inline]
    fn des(raw: &[u8]) -> Result<Self> {
        let mut res = [T::default(); CNT];
        for (ix, raw) in raw.chunks(WIDTH).enumerate() {
            res[ix] = FixedTV::des(raw)?.0;
        }
        Ok(Self(res))
    }

    fn ser(&self, raw: &mut [u8]) {
        for (val, chunk) in self.0.iter().zip(raw.chunks_mut(WIDTH)) {
            <FixedTV<T, WIDTH> as Parsergen>::ser(&FixedTV::new(*val), chunk);
        }
    }

    fn slice(_raw: &[u8], _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
*/
