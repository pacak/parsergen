use proc_macro2::TokenStream;
use quote::*;
use std::iter::FromIterator;
use syn::{parse::Parse, punctuated::Punctuated, *};

#[derive(Debug)]
pub enum Annotation {
    Literal(Literal),
    Fixed(usize),
    ViaIso(Box<Type>),
    Offset(LitInt),
}

impl parse::Parse for Annotation {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let tname = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        if tname == "decimal" {
            let lit: LitInt = input.parse()?;
            Ok(Annotation::Fixed(lit.base10_parse()?))
        } else if tname == "literal" {
            Ok(Annotation::Literal(input.parse::<AsciiLiteral>()?.0))
        } else if tname == "hex" {
            Ok(Annotation::Literal(input.parse::<HexLiteral>()?.0))
        } else if tname == "via" {
            Ok(Annotation::ViaIso(Box::new(input.parse()?)))
        } else if tname == "offset" {
            Ok(Annotation::Offset(input.parse()?))
        } else {
            Err(Error::new(tname.span(), "unexpected annotation"))
        }
    }
}

#[derive(Debug)]
pub struct Literal(Vec<u8>);

impl Literal {
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl ToTokens for Literal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        token::Bracket::default().surround(tokens, |t| {
            Punctuated::<_, Token![,]>::from_iter(self.0.iter()).to_tokens(t)
        })
    }
}

/// used for hex literal annotation, #[parsergen(hex: "DEADBEEF")]
pub struct HexLiteral(pub Literal);
impl Parse for HexLiteral {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let hex_digit = |i: u8| match i as char {
            '0'..='9' => Ok(i - b'0'),
            'A'..='F' => Ok(i - b'A' + 10),
            'a'..='f' => Ok(i - b'a' + 10),
            _ => Err(Error::new(input.span(), "invalid hex literal")),
        };
        let hex_char = |i: &[u8]| Ok(hex_digit(i[0])? * 16 + hex_digit(i[1])?);
        Ok(HexLiteral(Literal(
            input
                .parse::<LitStr>()?
                .value()
                .as_bytes()
                .chunks(2)
                .map(hex_char)
                .collect::<Result<Vec<u8>>>()?,
        )))
    }
}

/// used for literal ASCII annotations, #[parsergen(literal: "B6021")]
pub struct AsciiLiteral(pub Literal);
impl Parse for AsciiLiteral {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(AsciiLiteral(Literal(
            input
                .parse::<LitStr>()?
                .value()
                .as_bytes()
                .iter()
                .copied()
                .collect::<Vec<u8>>(),
        )))
    }
}
