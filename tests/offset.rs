use parsergen::*;

#[derive(Debug, Parsergen)]
struct Foo {
    #[parsergen(decimal: 2, offset: 0)]
    a: usize,
    #[parsergen(offset: 2)]
    b: Filler<3>,
}
