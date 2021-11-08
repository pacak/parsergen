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

#[test]
fn x0() {
    roundtrip(b"1234", X0 { val: 1234 })
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Default, Parsergen)]
struct X0s {
    a: X0,
    b: X0,
}

#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X0a {
    val: [X0; 2],
}

#[test]
fn x0a() {
    roundtrip(
        b"12345678",
        X0a {
            val: [X0 { val: 1234 }, X0 { val: 5678 }],
        },
    );
}
