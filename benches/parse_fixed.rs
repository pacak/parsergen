use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use parsergen::*;

fn unsigned_decimals(c: &mut Criterion) {
    let payload = b"1234567890123456789012345678901234567890";
    macro_rules! check {
        ($ty:ty, $($len:literal),+) => {$({


            pub fn fold_digits<T>(digits: &[u8]) -> Option<T>
            where
                T: std::ops::Mul<Output = T> + std::ops::Add<Output = T> + From<u8>,
            {
                let mut acc: T = 0.into();
                for d in digits.iter() {
                    let d = d.wrapping_sub(b'0');
                    if d >= 10 {
                        return None;
                    }
                    acc = acc * 10.into() + d.into();
                }
                Some(acc)
            }

            let input: [u8; $len] = payload[..$len].try_into().unwrap();
            let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
            let mut group =
                c.benchmark_group(format!("Unsigned {:02?} {}", $len, stringify!($ty)));

            group.bench_with_input(id("fixed"), &input, |b, &s| {
                b.iter(|| {
                    let a0 = black_box(parse_fixed!($ty, $len)(s));
                    let a1 = black_box(parse_fixed!($ty, $len)(s));
                    let a2 = black_box(parse_fixed!($ty, $len)(s));
                    let a3 = black_box(parse_fixed!($ty, $len)(s));
                    let a4 = black_box(parse_fixed!($ty, $len)(s));
                    let a5 = black_box(parse_fixed!($ty, $len)(s));
                    let a6 = black_box(parse_fixed!($ty, $len)(s));
                    let a7 = black_box(parse_fixed!($ty, $len)(s));
                    let a8 = black_box(parse_fixed!($ty, $len)(s));
                    let a9 = black_box(parse_fixed!($ty, $len)(s));
                    black_box((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
                });
            });

            group.bench_with_input(id("fold"), &input, |b, &s| {
                b.iter(|| {
                    let a0 = black_box(fold_digits::<$ty>(&s));
                    let a1 = black_box(fold_digits::<$ty>(&s));
                    let a2 = black_box(fold_digits::<$ty>(&s));
                    let a3 = black_box(fold_digits::<$ty>(&s));
                    let a4 = black_box(fold_digits::<$ty>(&s));
                    let a5 = black_box(fold_digits::<$ty>(&s));
                    let a6 = black_box(fold_digits::<$ty>(&s));
                    let a7 = black_box(fold_digits::<$ty>(&s));
                    let a8 = black_box(fold_digits::<$ty>(&s));
                    let a9 = black_box(fold_digits::<$ty>(&s));
                    black_box((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
                });
            });
        })+};
    }
    /*
    check!(u8, 1, 3);

    check!(u16, 1, 3, 5);

    check!(u32, 1, 3, 5, 8, 9);

    check!(u64, 1, 3, 5, 8, 12, 16, 19);

    check!(usize, 1, 3, 5, 8, 12, 16, 19);

    check!(u128, 1, 3, 5, 8, 12, 16, 19, 24, 26, 32, 36);*/
}

fn signed_decimals(c: &mut Criterion) {
    let payload = b"-1234567890123456789012345678901234567890";
    macro_rules! check {
        ($ty:ty, $($len:literal),+) => {$({


            let input: [u8; $len] = payload[..$len].try_into().unwrap();
            let id = |n| BenchmarkId::new(n, std::str::from_utf8(&input).unwrap());
            let mut group =
                c.benchmark_group(format!("Signed {:02?} {}", $len, stringify!($ty)));

            group.bench_with_input(id("fixed"), &input, |b, &s| {
                b.iter(|| {
                    let a0 = black_box(parse_fixed!($ty, $len)(s));
                    let a1 = black_box(parse_fixed!($ty, $len)(s));
                    let a2 = black_box(parse_fixed!($ty, $len)(s));
                    let a3 = black_box(parse_fixed!($ty, $len)(s));
                    let a4 = black_box(parse_fixed!($ty, $len)(s));
                    let a5 = black_box(parse_fixed!($ty, $len)(s));
                    let a6 = black_box(parse_fixed!($ty, $len)(s));
                    let a7 = black_box(parse_fixed!($ty, $len)(s));
                    let a8 = black_box(parse_fixed!($ty, $len)(s));
                    let a9 = black_box(parse_fixed!($ty, $len)(s));
                    black_box((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9));
                });
            });
        })+};
    }
    /*
    check!(i8, 2, 4);

    check!(i16, 2, 4, 6);

    check!(i32, 2, 4, 6, 9, 10);

    check!(i64, 2, 4, 6, 9, 13, 17, 20);

    check!(isize, 2, 4, 6, 9, 13, 17, 20);

    check!(i128, 2, 4, 6, 9, 13, 16, 20, 25, 27, 33, 37);*/
}

criterion_group! {
    name = digits;
    config = Criterion::default();
    targets = unsigned_decimals, signed_decimals,
}

criterion_main!(digits);
