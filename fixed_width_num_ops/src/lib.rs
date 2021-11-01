use proc_macro2::TokenStream;
use quote::*;
use syn::{parse::Parse, spanned::Spanned, *};

/// parse_fixed!(u32, 12[!])
/// produces a fn(input: [u8; 12]) -> Option<u32>
#[proc_macro]
pub fn parse_fixed(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let x = parse_fixed_impl(parse_macro_input!(input as ParseInput)).into();
    println!("{}", x);
    x
}

struct ParseInput {
    ty: Type,
    style: Style,
    width: usize,
}

enum Style {
    Signed(usize, TokenStream),
    Unsigned(usize),
}

impl Parse for ParseInput {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let ty = input.parse::<Type>()?;
        input.parse::<Token![,]>()?;
        let width = input.parse::<LitInt>()?.base10_parse()?;
        let style = get_style(&ty)?;
        Ok(Self { ty, style, width })
    }
}

fn get_style(ty: &Type) -> Result<Style> {
    match ty {
        Type::Group(TypeGroup { elem: ty, .. }) => get_style(ty),
        Type::Path(TypePath { path: p, .. }) => Ok(if p.is_ident("u8") {
            Style::Unsigned(1)
        } else if p.is_ident("u16") {
            Style::Unsigned(2)
        } else if p.is_ident("u32") {
            Style::Unsigned(4)
        } else if p.is_ident("u64") {
            Style::Unsigned(8)
        } else if p.is_ident("usize") {
            Style::Unsigned(8)
        } else if p.is_ident("u128") {
            Style::Unsigned(16)
        } else if p.is_ident("i8") {
            Style::Signed(1, quote!(u8))
        } else if p.is_ident("i16") {
            Style::Signed(2, quote!(u16))
        } else if p.is_ident("i32") {
            Style::Signed(4, quote!(u32))
        } else if p.is_ident("i64") {
            Style::Signed(8, quote!(u64))
        } else if p.is_ident("isize") {
            Style::Signed(8, quote!(usize))
        } else if p.is_ident("i128") {
            Style::Signed(16, quote!(u128))
        } else {
            return Err(Error::new(ty.span(), "unsupported type"));
        }),
        _ => Err(Error::new(ty.span(), "unsupported type")),
    }
}

fn parse_unsigned_bit(width: usize, ty: TokenStream) -> impl Fn(&mut usize) -> Option<TokenStream> {
    move |offset| {
        let rest = width - *offset;
        if rest >= 8 {
            let mul = 10u128.pow((width - *offset - 8) as u32);
            let s = quote! {{
                let raw = unsafe { as_array(&input[#offset .. #offset + 8]) };

                let v = u64::from_le_bytes(*raw);
                const ADD: u64 = u64::from_le_bytes([b'F'; 8]);
                const SUB: u64 = u64::from_le_bytes([b'0'; 8]);
                const MASK: u64 = u64::from_le_bytes([0x80; 8]);
                let a = v.wrapping_add(ADD);
                let v = v.wrapping_sub(SUB);
                if (a | v) & MASK == 0 {
                    let v = v.wrapping_mul(2561) >> 8;
                    let v = (v & 0x00FF00FF00FF00FF).wrapping_mul(6553601) >> 16;
                    let v = (v & 0x0000FFFF0000FFFF).wrapping_mul(42949672960001) >> 32;
                    val += v as #ty * #mul as #ty;
                } else {
                    return None
                }
            }};
            *offset += 8;
            Some(s)
        } else if rest >= 4 {
            let mul = 10u128.pow((width - *offset - 4) as u32);
            let s = quote! {{
                let raw = unsafe { as_array(&input[#offset .. #offset + 4]) };

                let v = u32::from_le_bytes(*raw);
                const ADD: u32 = u32::from_le_bytes([b'F'; 4]);
                const SUB: u32 = u32::from_le_bytes([b'0'; 4]);
                const MASK: u32 = u32::from_le_bytes([0x80; 4]);
                let a = v.wrapping_add(ADD);
                let v = v.wrapping_sub(SUB);
                if (a | v) & MASK == 0 {
                    let v = (v.wrapping_mul(2561)) >> 8;
                    let v = (v & 0x00FF00FF).wrapping_mul(6553601) >> 16;
                    val += v as #ty * #mul as #ty;
                } else {
                    return None
                }}
            };
            *offset += 4;
            Some(s)
        } else if rest == 3 {
            let s = quote! {{
                let raw = [b'0', input[#offset], input[#offset + 1], input[#offset + 2]];

                let v = u32::from_le_bytes(raw);
                const ADD: u32 = u32::from_le_bytes([b'F'; 4]);
                const SUB: u32 = u32::from_le_bytes([b'0'; 4]);
                const MASK: u32 = u32::from_le_bytes([0x80; 4]);
                let a = v.wrapping_add(ADD);
                let v = v.wrapping_sub(SUB);
                if (a | v) & MASK == 0 {
                    let v = (v.wrapping_mul(2561)) >> 8;
                    let v = (v & 0x00FF00FF).wrapping_mul(6553601) >> 16;
                    val += v as #ty;
                } else {
                    return None
                }
            }};
            *offset = width;
            Some(s)
        } else if rest == 2 {
            let s = quote! {{
                let raw = [input[#offset], input[#offset + 1]];

                let v = u16::from_le_bytes(raw);
                const ADD: u16 = u16::from_le_bytes([b'F'; 2]);
                const SUB: u16 = u16::from_le_bytes([b'0'; 2]);
                const MASK: u16 = u16::from_le_bytes([0x80; 2]);
                let a = v.wrapping_add(ADD);
                let mut v = v.wrapping_sub(SUB);
                if (a | v) & MASK == 0 {
                    v = v.wrapping_mul(2561) >> 8;
                    val += v as #ty;
                } else {
                    return None
                }
            }};
            *offset = width;
            Some(s)
        } else if rest == 1 {
            let s = quote! {{
                let v = input[#offset].wrapping_sub(b'0');
                if v < 10 {
                    val += v as #ty
                } else {
                    return None
                }
            }};
            *offset = width;
            Some(s)
        } else {
            None
        }
    }
}

fn parse_fixed_impl(ParseInput { ty, style, width }: ParseInput) -> TokenStream {
    let parsers = match style {
        Style::Signed(_, uty) => {
            let body = unfold(1, parse_unsigned_bit(width, uty));
            let res = quote! {
                use std::ops::Neg;
                match input[0] {
                    b' ' | b'0' => Some(val as #ty),
                    b'-' => Some((val as #ty).neg()),
                    _ => None
                }
            };
            body.chain(Some(res))
        }
        Style::Unsigned(_) => {
            let uty = quote!(#ty);
            let body = unfold(0, parse_unsigned_bit(width, uty));
            let res = quote!(Some(val));
            body.chain(Some(res))
        }
    };
    quote!( |input: [u8; #width]| {
        unsafe fn as_array<const N: usize>(slice: &[u8]) -> &[u8; N] {
            &*(slice.as_ptr() as *const [u8; N])
        }
        let mut val = 0;
        #(#parsers)*
    })
}

fn unfold<A, St, F>(initial_state: St, f: F) -> Unfold<St, F>
where
    F: FnMut(&mut St) -> Option<A>,
{
    Unfold {
        f,
        state: initial_state,
    }
}

struct Unfold<St, F> {
    f: F,
    pub state: St,
}

impl<A, St, F> Iterator for Unfold<St, F>
where
    F: FnMut(&mut St) -> Option<A>,
{
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        (self.f)(&mut self.state)
    }
}