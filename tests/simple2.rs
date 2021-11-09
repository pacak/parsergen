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

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X5(#[parsergen(literal: "Hello")] ());

#[test]
fn x5() {
    roundtrip(b"Hello", X5(()));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X6 {
    #[parsergen(hex: "a1A203")]
    val: (),
}

#[test]
fn x6() {
    roundtrip(&[0xa1, 0xa2, 3], X6 { val: () });
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X15(#[parsergen(decimal: 11)] u32);

#[test]
fn x15() {
    roundtrip::<Option<X15>, 11>(b"           ", None);
    roundtrip::<Option<X15>, 11>(b"00000000001", Some(X15(1)));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X15i(#[parsergen(decimal: 11)] i32);

#[test]
fn x15i() {
    roundtrip::<Option<X15>, 11>(b"           ", None);
    roundtrip::<Option<X15>, 11>(b"00000000001", Some(X15(1)));
}

#[test]
fn cents() {
    roundtrip::<Cents<8>, 8>(b" 1234.56", Cents::from(123456));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X16(#[parsergen(via: Cents<5>)] i64);

#[test]
fn x16() {
    roundtrip(b" 1.23", X16(123));
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X17(Blob<4>);

#[test]
fn x17() {
    roundtrip(b"1234", X17(Blob(*b"1234")));
}
