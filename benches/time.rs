use criterion::{black_box, criterion_group, criterion_main, Criterion};

use parsergen::time;

macro_rules! read {
    ($fn_r:ident, $fn_w:ident, $payload:literal) => {
        fn $fn_r(c: &mut Criterion) {
            let input = $payload;
            c.bench_function(stringify!($fn_r), |b| {
                b.iter(|| time::$fn_r(black_box(input)).unwrap())
            });
        }
        /*
        fn $fn_w(c: &mut Criterion) {
            let mut input = *$payload;
            let val = time::$fn_r(&input).unwrap();
            c.bench_function(stringify!($fn_w), |b| {
                b.iter(|| time::$fn_w(black_box(val), &mut input))
            });
        }*/
    };
}

read!(read_time6, write_time6, b"123456");
read!(read_time8, write_time8, b"12345601");
read!(read_time12, write_time12, b"123456012345");

criterion_group!(
    time,
    read_time6,
    read_time8,
    read_time12,
    //    write_time6,
    //    write_time8,
    //    write_time12,
);
criterion_main!(time);
