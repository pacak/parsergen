use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use parsergen::primitives::*;
use parsergen::time::{read_time12, write_time12};
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
struct FutDir {
    #[parsergen(decimal: 6)]
    total: u32,
    best5: [PQPairFut; 5],
}

#[derive(Parsergen, Copy, Clone, Default, Debug)]
struct FutPrices {
    bids: FutDir,
    asks: FutDir,

    #[parsergen(decimal: 5)]
    valid_bids: u32,
    #[parsergen(decimal: 4)]
    valid_bids_lvl: [u32; 5],

    #[parsergen(decimal: 5)]
    valid_asks: u32,
    #[parsergen(decimal: 4)]
    valid_asks_lvl: [u32; 5],
}

#[derive(Copy, Clone, Debug)]
struct Isin(u64);

impl HasWidth for Isin {
    const WIDTH: usize = 12;
}

impl Parsergen<12> for Isin {
    fn des(raw: &[u8; 12]) -> Result<Self, Error> {
        Ok(Isin(fold_isin(*raw).unwrap().0))
    }

    fn ser(&self, raw: &mut [u8; 12]) {
        let buf = unfold_isin(ISIN(self.0));
        raw.copy_from_slice(&buf);
    }
}

#[derive(Debug, PartialEq, Copy, Clone, Eq, Parsergen)]
pub enum BoardId {
    #[parsergen(literal: "G1")]
    RegularSession,
    /// Pre-Off Hours Session Closing Price
    #[parsergen(literal: "G2")]
    G2,
    /// Post-Off Hours Session Closing Price
    #[parsergen(literal: "G3")]
    G3,
    /// Post-Off Hours Session Single Price
    #[parsergen(literal: "G4")]
    G4,
    /// Pre-Off Hours Session Closing Price
    #[parsergen(literal: "G5")]
    G5,
    /// Regular Buy-In
    #[parsergen(literal: "G6")]
    G7,
    /// Same day Buy-In
    #[parsergen(literal: "G8")]
    G8,
    /// Pre-Off Hours Session
    ///
    /// Pre-Market Negotiated Trade
    #[parsergen(literal: "U2")]
    U2,
    #[parsergen(literal: "N1")]
    N1,
}

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

impl HasWidth for Time12 {
    const WIDTH: usize = 12;
}

impl Parsergen<12> for Time12 {
    fn des<'a>(raw: &'a [u8; 12]) -> Result<Self, Error> {
        Ok(Time12(read_time12(raw).unwrap()))
    }

    fn ser(&self, res: &mut [u8; 12]) {
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
    board_id: BoardId,
    session_id: SessionId,
    prices: FutPrices,
    #[parsergen(via: Time12)]
    timestamp: ExchangeTime,
    #[parsergen(decimal: 6)]
    estimated_price: i32,
}

/*
fn b_pqpair_arr(c: &mut Criterion) {
    let input = b" 12345123456 12345123456 12345123456 12345123456 12345123456";
    c.bench_function("PQPairFut pvec x 1", |b| {
        b.iter(|| {
            PQPairFut::des(black_box(&input[..12])).unwrap();
        })
    });

    c.bench_function("PQPairFut pvec x 5 ", |b| {
        b.iter(|| {
            <[PQPairFut; 5]>::des(black_box(input)).unwrap();
        })
    });
}*/

fn b_cents(c: &mut Criterion) {
    let input = b" 00001234.56";
    c.bench_function("cents 12", |b| {
        b.iter(|| {
            Cents::<12>::des(black_box(input)).unwrap();
        })
    });
}

fn b_fold_isin(c: &mut Criterion) {
    let input = b"AU0000XVGZA3";
    c.bench_function("fold_isin", |b| {
        b.iter(|| {
            fold_isin(black_box(*input)).unwrap();
        })
    });
}

fn b_unfold_isin(c: &mut Criterion) {
    let input = fold_isin(*b"AU0000XVGZA3").unwrap();
    c.bench_function("unfold_isin", |b| {
        b.iter(|| {
            unfold_isin(black_box(input));
        })
    });
}
fn des_pq2(raw: &[u8; 12]) -> Option<PQPairFut> {
    let mut buf = [0; 16];
    buf[..12].copy_from_slice(raw);
    let (_mask, [hi, lo]) = primitives::read_decimal(buf);
    Some(PQPairFut {
        price: hi as i32,
        qty: lo as u32,
    })
}

fn b_fut_pairs_vec(c: &mut Criterion) {
    let input = b" 12345123456";
    let input = black_box(input);
    c.bench_function("PQPairFut vec", |b| b.iter(|| des_pq2(input).unwrap()));
}

fn b_fut_pairs_vec_u(c: &mut Criterion) {
    let input = b" 12345123456";
    let input = black_box(input);
    c.bench_function("PQPairFut vec copy paste", |b| {
        b.iter(|| {
            {
                let mut buf = [0; 16];
                buf[..12].copy_from_slice(input);
                let (_mask, [hi, lo]) = unsafe {
                    use core::arch::x86_64::*;
                    use std::intrinsics::transmute;
                    let ascii_digits = transmute::<[u8; 16], __m128i>(buf);
                    let offset = _mm_set1_epi8((b'0' + 128) as i8);
                    let shifted_digits = _mm_sub_epi8(ascii_digits, offset);
                    let high_bound = _mm_set1_epi8(-128 + 10);
                    let mask = _mm_cmpgt_epi8(high_bound, shifted_digits);
                    let digits_mask = _mm_movemask_epi8(mask);

                    let chunk = transmute(buf);
                    let zeros = _mm_set1_epi8(b'0' as i8);
                    let chunk = _mm_sub_epi8(chunk, zeros);

                    //        let mult = _mm_set_epi8(1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10);
                    let mult = _mm_set1_epi16(0x010a);
                    let chunk = _mm_maddubs_epi16(chunk, mult);

                    let mult = _mm_set_epi16(1, 100, 1, 100, 1, 100, 1, 100);
                    let chunk = _mm_madd_epi16(chunk, mult);

                    let chunk = _mm_packus_epi32(chunk, chunk);
                    let mult = _mm_set_epi16(0, 0, 0, 0, 1, 10000, 1, 10000);
                    let chunk = _mm_madd_epi16(chunk, mult);

                    let re: [u32; 4] = transmute(chunk);
                    let hi = re[0] as u64;
                    let lo = re[1] as u64;

                    (digits_mask, [hi, lo])
                };
                PQPairFut {
                    price: hi as i32,
                    qty: lo as u32,
                }
            }
        })
    });
}

fn b_fut_pairs(c: &mut Criterion) {
    let input = b" 12345123456";
    c.bench_function("PQPairFut", |b| {
        b.iter(|| {
            PQPairFut::des(black_box(input)).unwrap();
        })
    });
}

fn b_fut_dir(c: &mut Criterion) {
    let input = b"000000 00000000182 00000000000 00000000000 00000000000 00000000000";
    c.bench_function("Fut Dir", |b| {
        b.iter(|| {
            FutDir::des(black_box(input)).unwrap();
        })
    });
}

fn b_fut_prices(c: &mut Criterion) {
    let input = b"000000 00000000182 00000000000 00000000000 00000000000 00000000000119255 00000305343 00000119275 46592000000 15184000000 0000000000000000179200000000000000000000046590000000000000000";
    c.bench_function("Fut Prices", |b| {
        b.iter(|| {
            FutPrices::des(black_box(input)).unwrap();
        })
    });
}

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
    b_fold_isin,
    b_unfold_isin,
    b_cents,
    b_fut_pairs,
    b_fut_pairs_vec,
    b_fut_pairs_vec_u,
    b_fut_dir,
    b_fut_prices,
    b_fut,
);
criterion_main!(primitive);
