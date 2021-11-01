#[cfg(test)]
mod tests;

use parsergen::*;
#[derive(Parsergen)]
pub struct NumbersGBench16 {
    #[parsergen(decimal: 14)]
    val1: u64,
    #[parsergen(decimal: 14)]
    val2: u64,
}
