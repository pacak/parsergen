use fixed_width_num_ops::*;

#[test]
fn unsigned() {
    let mut payload = Vec::new();
    for i in 1..40 {
        payload.push((i % 10) as u8 + b'0')
    }
    macro_rules! check {
        ($ty:ty, $($w:literal),+) => {$({
            let mut payload: [u8; $w] = payload[..$w].try_into().unwrap();
            let s = std::str::from_utf8(&payload).unwrap();
            let ex = s.parse::<$ty>().ok();

            println!("trying {:?}", s);
            assert_eq!(
                parse_fixed!($ty, $w)(payload),
                ex,
                "while parsing {:?} as {}",
                s,
                stringify!($ty)
            );

            payload[0] = b'x';
            assert_eq!(parse_fixed!($ty, $w)(payload), None);

        })+};
    }

    check!(u8, 1, 2, 3);

    check!(u16, 1, 2, 3, 4, 5);

    check!(u32, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    check!(u64, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check!(u64, 11, 12, 13, 14, 15, 16, 17, 18, 19);

    check!(usize, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check!(usize, 11, 12, 13, 14, 15, 16, 17, 18, 19);

    check!(u128, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check!(u128, 11, 12, 13, 14, 15, 16, 17, 18, 19);
    check!(u128, 21, 22, 23, 24, 25, 26, 27, 28, 29);
    check!(u128, 31, 32, 33, 34, 35, 36, 37, 38);
}

#[test]
fn signed() {
    let mut payload = Vec::new();
    payload.push(b'-');
    for i in 1..40 {
        payload.push((i % 10) as u8 + b'0')
    }
    macro_rules! check {
        ($ty:ty, $($w:literal),+) => {$({
            let mut payload: [u8; $w] = payload[..$w].try_into().unwrap();
            let s = std::str::from_utf8(&payload).unwrap();
            let ex = s.parse::<$ty>().ok();

            println!("trying {:?}", s);
            assert_eq!(
                parse_fixed!($ty, $w)(payload),
                ex,
                "while parsing {:?} as {}",
                s,
                stringify!($ty)
            );

            payload[1] = b'x';
            assert_eq!(
                parse_fixed!($ty, $w)(payload),
                None
            );

        })+};
    }

    check!(i8, 2, 3, 4);

    check!(i16, 2, 3, 4, 5, 6);

    check!(i32, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    check!(i64, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check!(i64, 11, 12, 13, 14, 15, 16, 17, 18, 19);

    check!(isize, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check!(isize, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);

    check!(i128, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check!(i128, 11, 12, 13, 14, 15, 16, 17, 18, 19);
    check!(i128, 21, 22, 23, 24, 25, 26, 27, 28, 29);
    check!(i128, 31, 32, 33, 34, 35, 36, 37, 38, 39);
}
