use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use parsergen::*;

fn unsigned_decimals(c: &mut Criterion) {
    let mut payload = Vec::new();
    for i in 1..500 {
        payload.push((i % 10) as u8 + b'0')
    }
    use parsergen::primitives::numbers::*;
    macro_rules! check {
        ($len:literal, $ident:ident, $fty:ty) => {
            #[derive(Parsergen, Default)]
            struct $ident {
                #[parsergen(decimal: $len)]
                val: $fty,
            }

            let input: [u8; $len] = payload[..$len].try_into().unwrap();
            let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
            let mut group =
                c.benchmark_group(format!("unsigned {:02?} {}", $len, stringify!($fty)));

            group.bench_with_input(id("composite"), &input, |b, &s| {
                b.iter(|| {
                    let _a = black_box(<$ident>::des(&s).unwrap_or_else(|_| <$ident>::default()));
                });
            });

            group.bench_with_input(id("fold"), &input, |b, &s| {
                b.iter(|| {
                    let _a = black_box(fold_digits::<$fty>(&s).unwrap_or(0));
                });
            });
            drop(group);
        };
    }

    check!(1, NumbersBench1, u32);
    check!(3, NumbersBench3, u32);
    check!(4, NumbersBench4, u32);
    check!(5, NumbersBench5, u32);

    check!(8, NumbersBench8, u64);
    check!(16, NumbersBench16, u64);
    check!(17, NumbersBench17, u64);
    check!(25, NumbersBench25q, u64);
    check!(26, NumbersBench26q, u64);

    check!(25, NumbersBench25, u128);
    check!(26, NumbersBench26, u128);
    check!(27, NumbersBench27, u128);
    check!(30, NumbersBench30, u128);
    check!(35, NumbersBench35, u128);
    check!(37, NumbersBench37, u128);
}

fn unsigned_decimal_groups(c: &mut Criterion) {
    let mut payload = Vec::new();
    for i in 1..500 {
        payload.push((i % 10) as u8 + b'0')
    }
    use parsergen::primitives::numbers::*;
    macro_rules! check {
        ($len:literal, $ident:ident, $fty:ty) => {
            #[derive(Parsergen, Default)]
            struct $ident {
                #[parsergen(decimal: $len)]
                val1: $fty,
                #[parsergen(decimal: $len)]
                val2: $fty,
            }

            let input: [u8; $len * 2] = payload[..$len * 2].try_into().unwrap();
            let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
            let mut group =
                c.benchmark_group(format!("unsigned group {:02?} {}", $len, stringify!($fty)));

            group.bench_with_input(id("composite"), &input, |b, &s| {
                b.iter(|| {
                    let _a = black_box(<$ident>::des(&s).unwrap_or_else(|_| <$ident>::default()));
                });
            });
            drop(group);
        };
    }

    check!(1, NumbersGBench1, u32);

    check!(3, NumbersGBench3, u32);
    check!(4, NumbersGBench4, u32);
    check!(5, NumbersGBench5, u32);

    check!(8, NumbersGBench8, u64);

    check!(16, NumbersGBench16, u64);
    check!(17, NumbersGBench17, u64);
    check!(25, NumbersGBench25q, u64);
    check!(26, NumbersGBench26q, u64);

    check!(25, NumbersGBench25, u128);
    check!(26, NumbersGBench26, u128);
    check!(27, NumbersGBench27, u128);
    check!(30, NumbersGBench30, u128);
    check!(35, NumbersGBench35, u128);
    check!(37, NumbersGBench37, u128);
}

fn experiment(c: &mut Criterion) {
    let mut payload = Vec::new();
    for i in 1..500 {
        payload.push((i % 10) as u8 + b'0')
    }
    use parsergen::primitives::numbers::*;

    ///////////////////////////////////////////////////////////////////////////////
    fn des<'a>(raw: &'a [u8; 3usize + 3usize]) -> Option<(u32, u32)> {
        const O_0: usize = 0;
        const O_1: usize = 0 + 3usize;
        const O_2: usize = O_1 + 3usize;
        let slice = ::arrayref::array_ref!(raw, O_0, O_1 - O_0);

        let mut val = 0;
        let a = ::arrayref::array_ref!(slice, 0usize, 3usize);
        let v = ::parsergen::primitives::numbers::fold_digits::<u32>(a)?;
        val += v as u32;
        let val1 = val;

        let slice = ::arrayref::array_ref!(raw, O_1, O_2 - O_1);

        let mut val = 0;
        let a = ::arrayref::array_ref!(slice, 0usize, 3usize);
        let v = ::parsergen::primitives::numbers::fold_digits::<u32>(a)?;
        val += v as u32;
        let val2 = val;
        Some((val1, val2))
    }
    ///////////////////////////////////////////////////////////////////////////////

    const LEN: usize = 3;

    let input: [u8; LEN * 2] = payload[..LEN * 2].try_into().unwrap();
    let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
    let mut group = c.benchmark_group("experiment");

    group.bench_with_input(id("composite"), &input, |b, &s| {
        b.iter(|| {
            let _a = black_box(des(&s));
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
