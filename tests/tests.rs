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

#[derive(Eq, PartialEq, Debug, Copy, Clone, Default, Parsergen)]
struct X0 {
    #[parsergen(decimal: 4)]
    val: u32,
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X1(#[parsergen(decimal: 4)] u32);

#[test]
fn x1() {
    roundtrip(b"0123", X1(123));
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
struct X7(#[parsergen(hex: "010203")] ());

#[test]
fn x7() {
    roundtrip(&[1, 2, 3], X7(()));
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
struct X13i {
    #[parsergen(decimal: 4)]
    i8_: i8,
    #[parsergen(decimal: 6, offset: 4)]
    i16_: i16,
    #[parsergen(decimal: 11)]
    i32_: i32,
    #[parsergen(decimal: 21)]
    i64_: i64,
    #[parsergen(decimal: 21)]
    isize_: isize,
}

#[test]
fn x13i() {
    let x = X13i {
        i8_: i8::MAX,
        i16_: i16::MAX,
        i32_: i32::MAX,
        i64_: i64::MAX,
        isize_: isize::MAX,
    };
    let msg = b" 127 32767 2147483647 09223372036854775807 09223372036854775807";

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
