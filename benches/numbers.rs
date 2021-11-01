use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use fixed_width_num_ops::*;
use parsergen::*;

fn unsigned_decimals(c: &mut Criterion) {
    let mut payload = Vec::new();
    for i in 1..500 {
        payload.push((i % 10) as u8 + b'0')
    }
    use parsergen::primitives::numbers::*;
    macro_rules! check {
        ($len:literal, $fty:ty) => {{
            let input: [u8; $len] = payload[..$len].try_into().unwrap();
            let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
            let mut group =
                c.benchmark_group(format!("unsigned single {:02?} {}", $len, stringify!($fty)));

            group.bench_with_input(id("fixed"), &input, |b, &s| {
                b.iter(|| {
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                    let _a = black_box(parse_fixed!($fty, $len)(s));
                });
            });

            group.bench_with_input(id("fold"), &input, |b, &s| {
                b.iter(|| {
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                    let _a = black_box(fold_digits::<$fty>(&s));
                });
            });
        }};
    }

    check!(1, u8);

    check!(2, u8);
    check!(3, u8);

    check!(1, u16);
    check!(3, u16);
    check!(4, u16);
    check!(5, u16);

    check!(1, u32);
    check!(3, u32);
    check!(4, u32);

    check!(5, u32);

    check!(1, u64);
    check!(3, u64);
    check!(4, u64);
    check!(5, u64);
    check!(8, u64);
    check!(16, u64);
    check!(17, u64);
    check!(25, u64);
    check!(26, u64);

    check!(25, u128);
    check!(26, u128);
    check!(27, u128);
    check!(30, u128);
    check!(35, u128);
    check!(37, u128);
}

fn check_things() {
    parse_fixed!(u8, 1)(*b"1");
    parse_fixed!(u8, 2)(*b"12");
    parse_fixed!(u8, 3)(*b"123");

    parse_fixed!(u16, 1)(*b"1");
    parse_fixed!(u16, 2)(*b"12");
    parse_fixed!(u16, 3)(*b"123");
    parse_fixed!(u16, 4)(*b"1234");
    parse_fixed!(u16, 5)(*b"12345");

    parse_fixed!(u32, 1)(*b"1");
    parse_fixed!(u32, 2)(*b"12");
    parse_fixed!(u32, 3)(*b"123");
    parse_fixed!(u32, 4)(*b"1234");
    parse_fixed!(u32, 5)(*b"12345");
    parse_fixed!(u32, 6)(*b"123456");
    parse_fixed!(u32, 7)(*b"1234567");
    parse_fixed!(u32, 8)(*b"12345678");

    parse_fixed!(u64, 1)(*b"1");
    parse_fixed!(u64, 2)(*b"12");
    parse_fixed!(u64, 3)(*b"123");
    parse_fixed!(u64, 4)(*b"1234");
    parse_fixed!(u64, 5)(*b"12345");
    parse_fixed!(u64, 6)(*b"123456");
    parse_fixed!(u64, 7)(*b"1234567");
    parse_fixed!(u64, 8)(*b"12345678");

    parse_fixed!(usize, 1)(*b"1");
    parse_fixed!(usize, 2)(*b"12");
    parse_fixed!(usize, 3)(*b"123");
    parse_fixed!(usize, 4)(*b"1234");
    parse_fixed!(usize, 5)(*b"12345");
    parse_fixed!(usize, 6)(*b"123456");
    parse_fixed!(usize, 7)(*b"1234567");
    parse_fixed!(usize, 8)(*b"12345678");

    parse_fixed!(u128, 1)(*b"1");
    parse_fixed!(u128, 2)(*b"12");
    parse_fixed!(u128, 3)(*b"123");
    parse_fixed!(u128, 4)(*b"1234");
    parse_fixed!(u128, 5)(*b"12345");
    parse_fixed!(u128, 6)(*b"123456");
    parse_fixed!(u128, 7)(*b"1234567");
    parse_fixed!(u128, 8)(*b"12345678");
}

fn unsigned_decimal_groups(c: &mut Criterion) {
    let mut payload = Vec::new();
    for i in 1..5000 {
        payload.push((i % 10) as u8 + b'0')
    }
    macro_rules! check {
        ($len:literal, $ident:ident, $fty:ty) => {{
            const MUL: usize = 5;
            #[derive(Parsergen)]
            struct $ident {
                #[parsergen(decimal: $len)]
                val1: $fty,
                #[parsergen(decimal: $len)]
                val2: $fty,
                #[parsergen(decimal: $len)]
                val3: $fty,
                #[parsergen(decimal: $len)]
                val4: $fty,
                #[parsergen(decimal: $len)]
                val5: $fty,
            }

            let input: [u8; $len * MUL] = payload[..$len * MUL].try_into().unwrap();
            let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
            let mut group = c.benchmark_group(format!(
                "unsigned group {:02?} x {} {}",
                $len,
                MUL,
                stringify!($fty)
            ));

            group.bench_with_input(id("composite"), &input, |b, &s| {
                b.iter(|| {
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                    let _a = black_box(<$ident>::des(&s));
                });
            });
        }};
    }
    /*
    check!(1, NumbersGBench1, u32);

    check!(3, NumbersGBench3, u32);
    check!(4, NumbersGBench4, u32);
    check!(5, NumbersGBench5, u32);

    check!(8, NumbersGBench8, u64);

    check!(12, NumbersGBench12, u64);
    check!(13, NumbersGBench13, u64);
    check!(14, NumbersGBench14, u64);
    check!(16, NumbersGBench16, u64);
    check!(17, NumbersGBench17, u64);
    check!(25, NumbersGBench25q, u64);
    check!(26, NumbersGBench26q, u64);

    check!(25, NumbersGBench25, u128);
    check!(26, NumbersGBench26, u128);
    check!(27, NumbersGBench27, u128);
    check!(30, NumbersGBench30, u128);
    check!(35, NumbersGBench35, u128);
    check!(37, NumbersGBench37, u128);*/
}

fn experiment(c: &mut Criterion) {
    let mut payload = Vec::new();
    for i in 1..500 {
        payload.push((i % 10) as u8 + b'0')
    }
    use parsergen::primitives::numbers::*;

    ///////////////////////////////////////////////////////////////////////////////
    struct NumbersGBench16 {
        val1: u64,
        val2: u64,
    }
    impl ::parsergen::HasWidth for NumbersGBench16 {
        const WIDTH: usize = 16usize + 16usize;
    }
    impl ::parsergen::Parsergen<{ 16usize + 16usize }> for NumbersGBench16 {
        fn des(raw: &[u8; 16usize + 16usize]) -> Option<Self> {
            const O_0: usize = 0;
            const O_1: usize = 0 + 16usize;
            const O_2: usize = O_1 + 16usize;
            let slice = ::arrayref::array_ref!(raw, O_0, O_1 - O_0);

            let mut val = 0;
            let a = ::arrayref::array_ref!(slice, 0usize, 8);
            let v = ::parsergen::primitives::numbers::parse_8(*a)?;
            val += v as u64 * 100000000u128 as u64;
            let a = ::arrayref::array_ref!(slice, 8usize, 8);
            let v = ::parsergen::primitives::numbers::parse_8(*a)?;
            val += v as u64 * 1u128 as u64;
            let val1 = val;

            let slice = ::arrayref::array_ref!(raw, O_1, O_2 - O_1);

            let mut val = 0;
            let a = ::arrayref::array_ref!(slice, 0usize, 8);
            let v = ::parsergen::primitives::numbers::parse_8(*a)?;
            val += v as u64 * 100000000u128 as u64;
            let a = ::arrayref::array_ref!(slice, 8usize, 8);
            let v = ::parsergen::primitives::numbers::parse_8(*a)?;
            val += v as u64 * 1u128 as u64;
            let val2 = val;
            Some(Self { val1, val2 })
        }
        fn ser(&self, raw: &mut [u8; 16usize + 16usize]) {
            const O_0: usize = 0;
            let Self { val1, val2 } = self;
            const O_1: usize = 0 + 16usize;
            const O_2: usize = O_1 + 16usize;
            let var = val1;
            let slice = ::arrayref::array_mut_ref!(raw, O_0, O_1 - O_0);
            <::parsergen::FixedT<u64, 16usize>>::new(*var).ser(slice);
            let var = val2;
            let slice = ::arrayref::array_mut_ref!(raw, O_1, O_2 - O_1);
            <::parsergen::FixedT<u64, 16usize>>::new(*var).ser(slice);
        }
    }

    ///////////////////////////////////////////////////////////////////////////////

    const LEN: usize = 16;

    let input: [u8; LEN * 2] = payload[..LEN * 2].try_into().unwrap();
    let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
    let mut group = c.benchmark_group("experiment");

    group.bench_with_input(id("composite"), &input, |b, &s| {
        b.iter(|| {
            let _a = black_box(NumbersGBench16::des(&s));
        });
    });
    drop(group);
}

criterion_group! {
    name = digits;
    config = Criterion::default();
    targets = unsigned_decimals, unsigned_decimal_groups, experiment
}

//criterion_group!(primitive, unsigned_decimals, unsigned_decimal_array);
criterion_main!(digits);
