use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parsergen::*;

fn unsigned_decimals(c: &mut Criterion) {
    let input = b"1234567890123456789012345678901234567890";

    macro_rules! check {
        ($width:literal) => {
            c.bench_function(&format!("unsigned decimal {} scalar", $width), |b| {
                b.iter(|| {
                    FixedT::<usize, $width>::des(black_box(&input[..$width])).unwrap();
                })
            });

            c.bench_function(&format!("unsigned decimal {} vectorized", $width), |b| {
                b.iter(|| {
                    FixedTV::<usize, $width>::des(black_box(&input[..$width])).unwrap();
                })
            });
        };
    }
    check!(1);
    check!(3);
    check!(4);
    check!(5);
    check!(8);
    check!(16);
    check!(17);
    check!(25);
    check!(37);
}

fn unsigned_decimal_array(c: &mut Criterion) {
    let input = b"12345678901234567890123456789012345678901234567890123456789012345678901234567890";

    macro_rules! check {
        ($width:literal) => {
            c.bench_function(&format!("unsigned decimal arr {} scalar", $width), |b| {
                b.iter(|| {
                    FixedArrT::<usize, $width, 2>::des(black_box(&input[..$width])).unwrap();
                })
            });

            c.bench_function(
                &format!("unsigned decimal arr {} vectorized", $width),
                |b| {
                    b.iter(|| {
                        FixedArrTV::<usize, $width, 2>::des(black_box(&input[..$width])).unwrap();
                    })
                },
            );
        };
    }
    check!(1);
    check!(3);
    check!(4);
    check!(5);
    check!(8);
    check!(16);
    check!(17);
    check!(25);
    check!(37);
}

criterion_group!(primitive, unsigned_decimals, unsigned_decimal_array);
criterion_main!(primitive);
