use parsergen::*;

#[derive(Parsergen)]
struct X {
    #[parsergen(decimal:5)]
    field: i64,
}

fn has_decor(s: &str) -> bool {
    s.bytes().any(|b| b == b'\x1b')
}

#[test]
fn fixed_working() {
    let x = Fixed::<1, 2, i64>::des(b"0.00");
    assert_eq!(x, Some(Fixed(0)));

    // wrong body
    assert!(has_decor(&format!(
        "{:?}",
        Sliced::<Fixed<2, 1, i64>, 4>::from(b"0x.0".as_slice()) // TODO, b"00.0" shouldn't be
                                                                // valid!
    )));

    assert_eq!(Fixed::<2, 1, i64>::des(b"01.2"), Some(Fixed(12)));
    assert_eq!(Fixed::<2, 1, i64>::des(b"0.12"), None);
    assert_eq!(Fixed::<1, 2, i64>::des(b"01.2"), None);
    assert_eq!(Fixed::<1, 2, i64>::des(b"0.12"), Some(Fixed(12)));

    // good
    assert!(!has_decor(&format!(
        "{:?}",
        Sliced::<Fixed<1, 2, i64>, 4>::from(b"0.00".as_slice())
    )));
}

#[test]
fn cents_working() {
    // wrong body
    assert!(has_decor(&format!(
        "{:?}",
        Sliced::<Cents<4>, 4>::from(b"00.0".as_slice())
    )));
    // good
    assert!(!has_decor(&format!(
        "{:?}",
        Sliced::<Cents<4>, 4>::from(b"0.00".as_slice())
    )));
}

#[test]
fn num_errors_are_labelled() {
    // wrong sign
    assert!(has_decor(&format!(
        "{:?}",
        Sliced::<X, 5>::from(b"x1234".as_slice())
    )));

    // wrong body
    assert!(has_decor(&format!(
        "{:?}",
        Sliced::<X, 5>::from(b" 1x34".as_slice())
    )));

    // good
    assert!(!has_decor(&format!(
        "{:?}",
        Sliced::<X, 5>::from(b" 1234".as_slice())
    )));
}

#[derive(Debug, Parsergen)]
enum Dir {
    #[parsergen(literal: "a")]
    Ask,
    #[parsergen(literal: "b")]
    Bid,
}

#[test]
fn literal_errors_are_labelled() {
    // wrong symbol
    assert!(has_decor(&format!(
        "{:?}",
        Sliced::<Dir, 1>::from(b"c".as_slice())
    )));

    // right symbol
    assert!(!has_decor(&format!(
        "{:?}",
        Sliced::<Dir, 1>::from(b"b".as_slice())
    )));
}

#[derive(Parsergen)]
struct Y {
    x: X,
    dir: Dir,
    #[parsergen(decimal: 2)]
    n: u32,
}

#[test]
fn errors_in_inherits_are_labelled() {
    // good
    assert!(!has_decor(&format!(
        "{:?}",
        Sliced::<Y, 8>::from(b" 1234b01".as_slice())
    )));

    // bad dir
    assert!(has_decor(&format!(
        "{:?}",
        Sliced::<Y, 8>::from(b" 1234d01".as_slice())
    )));
}
