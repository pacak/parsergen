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

//#[derive(Eq, PartialEq, Parsergen, Debug)]
struct X0a {
    val: [X0; 2],
}

impl ::parsergen::HasWidth for X0a {
    const WIDTH: usize = <X0 as ::parsergen::HasWidth>::WIDTH * 2;
}
impl ::parsergen::Parsergen<{ <X0 as ::parsergen::HasWidth>::WIDTH * 2 }> for X0a {
    fn des(raw: &[u8; <X0 as ::parsergen::HasWidth>::WIDTH * 2]) -> Option<Self> {
        const O_0: usize = 0;
        const O_1: usize = 0 + <X0 as ::parsergen::HasWidth>::WIDTH * 2;
        let slice = ::arrayref::array_ref!(raw, O_0, O_1 - O_0);
        let val = ::parsergen::des_array::<
            X0,
            { <X0 as ::parsergen::HasWidth>::WIDTH * 2 },
            { <X0 as ::parsergen::HasWidth>::WIDTH },
            2,
        >(slice)?;
        Some(Self { val })
    }
    fn ser(&self, raw: &mut [u8; <X0 as ::parsergen::HasWidth>::WIDTH * 2]) {
        const O_0: usize = 0;
        let Self { val } = self;
        const O_1: usize = 0 + <X0 as ::parsergen::HasWidth>::WIDTH * 2;
        let var = val;
        let slice = ::arrayref::array_mut_ref!(raw, O_0, O_1 - O_0);
        ::parsergen::ser_array::<
            X0,
            { <X0 as ::parsergen::HasWidth>::WIDTH * 2 },
            { <X0 as ::parsergen::HasWidth>::WIDTH },
            2,
        >(*var, slice);
    }
    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const O_0: usize = 0;
        const O_1: usize = 0 + <X0 as ::parsergen::HasWidth>::WIDTH * 2;
        let mut d = f.debug_struct("X0a");
        d.field(
            "val",
            &(|slice| {
                ::parsergen::slice_arr::<X0, { <X0 as ::parsergen::HasWidth>::WIDTH }>(slice, f)
            })(&raw[O_0..O_1]),
        );
        d.finish()
    }
}
