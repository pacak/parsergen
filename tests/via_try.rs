use parsergen::*;

#[derive(Debug, Clone, Copy, Parsergen)]
struct Z {
    #[parsergen(decimal: 4)]
    val: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Y {
    Alpha,
    Beta,
    Gamma,
}

impl TryFrom<Z> for Y {
    type Error = ();

    fn try_from(value: Z) -> Result<Self, Self::Error> {
        Ok(match value.val {
            0 => Self::Alpha,
            1 => Self::Beta,
            2 => Self::Gamma,
            _ => return Err(()),
        })
    }
}

impl From<Y> for Z {
    fn from(value: Y) -> Self {
        let val = match value {
            Y::Alpha => 0,
            Y::Beta => 1,
            Y::Gamma => 2,
        };
        Self { val }
    }
}

#[derive(Debug, Clone, Copy, Parsergen, Eq, PartialEq)]
struct X {
    #[parsergen(via: Z)]
    y: Y,
}

#[test]
fn asef() {
    assert_eq!(X::des(b"0000"), Some(X { y: Y::Alpha }));
    assert_eq!(X::des(b"0002"), Some(X { y: Y::Gamma }));
    assert_eq!(X::des(b"0004"), None);
}
