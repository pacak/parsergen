use std::num::TryFromIntError;

use parsergen::*;

#[track_caller]
fn roundtrip<T, const WIDTH: usize>(raw: &[u8; WIDTH], parsed: T)
where
    T: Parsergen<WIDTH> + std::fmt::Debug + Eq + PartialEq,
{
    let mut buf = [0u8; WIDTH];
    let r = T::des(raw).unwrap();
    println!("{:?}", r);
    T::ser(&parsed, &mut buf);
    assert_eq!(&buf, raw, "while testing encoder");
    assert_eq!(parsed, T::des(raw).unwrap(), "while testing decoder");
}

#[test]
fn can_parse_small_values() {
    roundtrip(b"0100", X { field: 100 });
    roundtrip(b"0255", X { field: 255 });
}

#[test]
fn cant_overflow_on_large_values() {
    assert_eq!(X::des(b"0256"), None);
}

#[derive(Eq, PartialEq, Debug, Parsergen)]
struct X {
    #[parsergen(via: Y)]
    field: u8,
}

#[derive(Eq, PartialEq, Debug, Parsergen)]
struct Y {
    #[parsergen(decimal: 4)]
    field: u16,
}

impl TryFrom<Y> for u8 {
    type Error = TryFromIntError;

    fn try_from(value: Y) -> Result<Self, Self::Error> {
        u8::try_from(value.field)
    }
}

impl From<u8> for Y {
    fn from(f: u8) -> Self {
        Y { field: f as u16 }
    }
}
