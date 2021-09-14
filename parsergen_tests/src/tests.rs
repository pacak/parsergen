use parsergen::*;

#[track_caller]
fn roundtrip<T>(raw: &[u8], parsed: T)
where
    T: Parsergen + std::fmt::Debug + Eq + PartialEq,
{
    let mut buf = raw.to_vec();
    T::ser(&parsed, &mut buf);
    assert_eq!(&buf, &raw);
    assert_eq!(parsed, T::des(raw).unwrap());
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X0 {
    #[parsergen(decimal: 4)]
    val: u32,
}

#[test]
fn x0() {
    roundtrip(b"1234", X0 { val: 1234 })
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X1(#[parsergen(decimal: 4)] u32);

#[test]
fn x1() {
    roundtrip(b"0123", X1(123));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
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

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X3(#[parsergen(decimal: 3)] [u32; 3]);

#[test]
fn x3() {
    roundtrip(b"123456789", X3([123, 456, 789]));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X4 {
    #[parsergen(literal: "Hello")]
    val: (),
}

#[test]
fn x4() {
    roundtrip(b"Hello", X4 { val: () });
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X5(#[parsergen(literal: "Hello")] ());

#[test]
fn x5() {
    roundtrip(b"Hello", X5(()));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X6 {
    #[parsergen(hex: "010203")]
    val: (),
}

#[test]
fn x6() {
    roundtrip(&[1, 2, 3], X6 { val: () });
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X7(#[parsergen(hex: "010203")] ());

#[test]
fn x7() {
    roundtrip(&[1, 2, 3], X7(()));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
#[parsergen(literal: "he")]
struct X8;

#[test]
fn x8() {
    roundtrip(b"he", X8);
}

impl From<X8> for () {
    fn from(_: X8) -> Self {
        ()
    }
}
impl From<()> for X8 {
    fn from(_: ()) -> Self {
        X8
    }
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X9 {
    #[parsergen(via: X8)]
    var: [(); 2],
}

#[test]
fn x9() {
    roundtrip(b"hehe", X9 { var: [(), ()] });
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
enum X10 {
    #[parsergen(literal: "lo")]
    F1,
    F2(X8),
}

#[test]
fn x10() {
    roundtrip(b"lo", X10::F1);
    roundtrip(b"he", X10::F2(X8));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
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

    panic!("{:?}", <Sliced<X11>>::from(msg));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X12 {
    #[parsergen(decimal: 3)]
    u8_: u8,
    #[parsergen(decimal: 5)]
    u16_: u16,
    #[parsergen(decimal: 10)]
    u32_: u32,
    #[parsergen(decimal: 20)]
    u64_: u64,
    #[parsergen(decimal: 20)]
    usize_: usize,
}

#[test]
fn x12() {
    let x = X12 {
        u8_: u8::MAX,
        u16_: u16::MAX,
        u32_: u32::MAX,
        u64_: u64::MAX,
        usize_: usize::MAX,
    };
    let msg = b"2556553542949672951844674407370955161518446744073709551615";
    roundtrip(msg, x);
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X13 {
    #[parsergen(decimal: 4)]
    u8_: u8,
    #[parsergen(decimal: 6)]
    u16_: u16,
    #[parsergen(decimal: 11)]
    u32_: u32,
    #[parsergen(decimal: 21)]
    u64_: u64,
    #[parsergen(decimal: 21)]
    usize_: usize,
}

#[test]
fn x13() {
    let x = X13 {
        u8_: u8::MAX,
        u16_: u16::MAX,
        u32_: u32::MAX,
        u64_: u64::MAX,
        usize_: usize::MAX,
    };
    let msg = b"025506553504294967295018446744073709551615018446744073709551615";
    roundtrip(msg, x);
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X14 {
    val: Option<X0>,
}

#[test]
fn x14() {
    roundtrip(b"    ", X14 { val: None });
    roundtrip(
        b"1234",
        X14 {
            val: Some(X0 { val: 1234 }),
        },
    );
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X15(#[parsergen(decimal: 11)] u32);

#[test]
fn x15() {
    roundtrip::<Option<X15>>(b"           ", None);
    roundtrip::<Option<X15>>(b"00000000001", Some(X15(1)));
}
