use iai::black_box;
use parsergen::*;

const PAYLOAD: &'static [u8; 50] = b"12345678901234567890123456789012345678901234567890";

macro_rules! check {
    ($len:literal, $ident:ident, $fty:ty, $fn:ident) => {
        #[derive(Parsergen, Default)]
        struct $ident {
            #[parsergen(decimal: $len)]
            val1: $fty,
            #[parsergen(decimal: $len)]
            val2: $fty,
        }

        fn $fn() {
            let input: [u8; $len * 2] = PAYLOAD[..$len * 2].try_into().unwrap();
            let _a = <$ident>::des(black_box(&input)).unwrap_or_else(|_| <$ident>::default());
        }
    };
}

/*
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
check!(37, NumbersBench37, u128);*/

check!(25, NumbersBench8, u32, check_16);

iai::main!(check_16);
