use criterion::{black_box, criterion_group, criterion_main, Criterion};

use parsergen::primitives::*;
use parsergen::*;

#[derive(Parsergen, Copy, Clone, Default, Debug)]
struct PQPair {
    #[parsergen(decimal: 5)]
    price: u32,
    #[parsergen(decimal: 7)]
    qty: u32,
}

#[derive(Parsergen, Debug)]
struct OptPrices {
    bids: [PQPair; 5],
    #[parsergen(decimal: 7)]
    total_bids: u32,
    asks: [PQPair; 5],
    #[parsergen(decimal: 7)]
    total_asks: u32,
}

#[derive(Parsergen, Copy, Clone, Default, Debug)]
struct PQPairFut {
    #[parsergen(decimal: 6)]
    price: i32,
    #[parsergen(decimal: 6)]
    qty: u32,
}

#[derive(Parsergen, Copy, Clone, Default, Debug)]
struct FutPrices {
    #[parsergen(decimal: 6)]
    total_bids: u32,

    bids: [PQPairFut; 5],

    #[parsergen(decimal: 6)]
    total_asks: u32,
    asks: [PQPairFut; 5],

    #[parsergen(decimal: 5)]
    valid_bids: u32,
    #[parsergen(decimal: 4)]
    valid_bids_lvl: [u32; 5],

    #[parsergen(decimal: 5)]
    valid_asks: u32,
    #[parsergen(decimal: 4)]
    valid_asks_lvl: [u32; 5],
}

#[derive(Parsergen, Copy, Clone, Debug)]
struct Isin([u8; 12]);

#[derive(Parsergen, Copy, Clone, Debug)]
enum SessionId {
    #[parsergen(literal: "00")]
    Initial,
    #[parsergen(literal: "10")]
    OpeningSinglePrice,
    #[parsergen(literal: "11")]
    OpeningSinglePriceExt,
    #[parsergen(literal: "20")]
    SinglePrice,
    #[parsergen(literal: "30")]
    ClosingSinglePrice,
    #[parsergen(literal: "40")]
    Continuous,
    #[parsergen(literal: "50")]
    IV,
    #[parsergen(literal: "51")]
    IVExt,
    #[parsergen(literal: "52")]
    OpeningIV,
    #[parsergen(literal: "53")]
    OpeningIVExt,
    #[parsergen(literal: "54")]
    ClosingIVPrice,
    #[parsergen(literal: "60")]
    ClosingPrice,
    #[parsergen(literal: "80")]
    UnitTrading,
    #[parsergen(literal: "90")]
    Halt,
    #[parsergen(literal: "98")]
    Shutdown,
    #[parsergen(literal: "99")]
    MarketClosing,
}

#[derive(Copy, Clone, Debug)]
struct Time12(u64);

impl Parsergen for Time12 {
    const WIDTH: usize = 12;

    fn des<'a>(raw: &'a [u8]) -> Result<Self> {
        Ok(Time12(read_time12(raw)?))
    }

    fn ser(&self, res: &mut [u8]) {
        write_time12(self.0, res)
    }

    fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", PrettyBytes(raw))
    }
}

impl From<Time12> for ExchangeTime {
    fn from(other: Time12) -> Self {
        Self(other.0)
    }
}

impl From<ExchangeTime> for Time12 {
    fn from(other: ExchangeTime) -> Self {
        Self(other.0)
    }
}

#[derive(Copy, Clone, Debug)]
struct ExchangeTime(u64);

#[derive(Parsergen, Copy, Clone, Debug)]
struct FutB6 {
    #[parsergen(literal: "B6014")]
    header: (),
    isin: Isin,
    #[parsergen(decimal: 8)]
    seq_no: u32,
    board_id: Filler<2>,
    session_id: SessionId,
    prices: FutPrices,
    #[parsergen(via: Time12)]
    timestamp: ExchangeTime,
    #[parsergen(decimal: 6)]
    estimated_price: i32,
}

fn b_unsigned_decimal(c: &mut Criterion) {
    let input = b"123456781234";
    c.bench_function("unsigned decimal", |b| {
        b.iter(|| {
            FixedT::<usize, 12>::des(black_box(input)).unwrap();
        })
    });
}

fn b_time(c: &mut Criterion) {
    let input = b"123456012345";
    c.bench_function("time12 native", |b| {
        b.iter(|| {
            read_time12_native(black_box(input)).unwrap();
        })
    });
}

fn b_time_select(c: &mut Criterion) {
    let input = b"123456012345";
    c.bench_function("time12", |b| {
        b.iter(|| {
            read_time12(black_box(input)).unwrap();
        })
    });
}

fn b_timev(c: &mut Criterion) {
    let input = b"123456012345";
    c.bench_function("time12_vec", |b| {
        b.iter(|| {
            read_time12_vec(black_box(input)).unwrap();
        })
    });
}

// goat to beat: 138ns
fn b_fut(c: &mut Criterion) {
    let input = b"B6014KR4101Q5000300000000G110000000 00000000182 00000000000 00000000000 00000000000 00000000000119255 00000305343 00000119275 46592000000 15184000000 0000000000000000179200000000000000000000046590000000000000000125632000000 00182";

    c.bench_function("FutB6", |b| {
        b.iter(|| {
            FutB6::des(black_box(input)).unwrap();
        })
    });
}

criterion_group!(
    primitive,
    b_unsigned_decimal,
    b_fut,
    b_time,
    b_time_select,
    b_timev,
);
criterion_main!(primitive);
