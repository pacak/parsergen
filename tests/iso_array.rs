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

#[derive(Parsergen, Eq, PartialEq, Debug)]
struct X2 {
    #[parsergen(decimal: 3)]
    val: [u32; 3],
}

#[test]
fn x2() {
    roundtrip(
        b"123456789",
        X2 {
            val: [123, 456, 789],
        },
    );
}

#[derive(Eq, PartialEq, Parsergen, Debug, Copy, Clone, Default)]
#[parsergen(literal: "he")]
/// doc
struct X8;

#[test]
fn x8() {
    roundtrip(b"he", X8);
}
#[derive(Eq, PartialEq, Debug, Parsergen)]
struct X9 {
    #[parsergen(via: X8)]
    var: [(); 2],
}
#[test]
fn x9() {
    roundtrip(b"hehe", X9 { var: [(), ()] });
}

impl From<()> for X8 {
    fn from(_: ()) -> Self {
        X8
    }
}

impl From<X8> for () {
    fn from(_: X8) -> Self {
        ()
    }
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
#[parsergen(literal: "hi")]
/// doc
struct X8a;

#[derive(Parsergen, Eq, PartialEq, Debug)]
enum X10 {
    #[parsergen(literal: "lo")]
    F1,
    F2(X8),
    F3 {
        x8a: X8a,
    },
}

#[test]
fn x10() {
    roundtrip(b"lo", X10::F1);
    roundtrip(b"he", X10::F2(X8));
    roundtrip(b"hi", X10::F3 { x8a: X8a });
}

#[derive(Eq, PartialEq, Debug, Parsergen)]
struct X11 {
    x2: X2,
    x10: X10,
}

#[test]
fn x11() {
    let x11 = X11 {
        x2: X2 {
            val: [123, 456, 789],
        },
        x10: X10::F1,
    };

    let msg = b"123456789lo";
    roundtrip(msg, x11);
}
