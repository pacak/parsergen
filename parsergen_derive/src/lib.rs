use proc_macro2::{Span, TokenStream};
use quote::*;
use std::iter::FromIterator;
use syn::{parse::Parse, punctuated::Punctuated, spanned::Spanned, *};

mod annotation;
use crate::primitives::{parse_fixed_arr_inner, parse_fixed_impl, parse_fixed_inner, ParseInput};

use self::annotation::*;

mod primitives;

#[proc_macro_derive(Parsergen, attributes(parsergen))]
pub fn derive_decode(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_decode_impl(parse_macro_input!(input as DecodeInput)) {
        Ok(ok) => ok.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// parse_fixed!(u32, 12[!])
/// produces a fn(input: [u8; 12]) -> Option<u32>
#[proc_macro]
pub fn parse_fixed(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let x = parse_fixed_impl(parse_macro_input!(input as ParseInput)).into();
    // println!("{}", x);
    x
}

fn derive_decode_impl(input: DecodeInput) -> Result<TokenStream> {
    let r = match input {
        DecodeInput::Struct(StructInput::Fields(x)) => for_struct_with_fields(x),
        DecodeInput::Struct(StructInput::Unit(x)) => for_unit_struct(x),
        DecodeInput::Enum(x) => for_enum(x),
    }?;
    //println!("{}", r);
    Ok(r)
}

fn for_enum(input: EnumInput) -> Result<TokenStream> {
    let EnumInput { ty, width, .. } = &input;
    let ty_str = ty.to_string();

    let checks = input.variants.iter().map(|f| match f {
        EnumVariant::UnitEnum(StructUnit { ty, literal, .. }) => {
            quote! {
                if raw == &#literal {
                    return Some(Self::#ty)
                }
            }
        }
        EnumVariant::EnumWithFields(sf) => {
            let ty = &sf.ty;
            let self_constr = sf.self_constr();
            let SeqField {
                name,
                ty: fty,
                parser,
                ..
            } = &sf.fields[0];
            quote! {
                #[allow(clippy::needless_question_mark)]
                // ? does nothing and it comes from unification between different parsers
                let tmp: Option<#fty> = (|slice|Some(#parser))(raw);
                match tmp {
                    None => {},
                    Some(#name) => return Some(Self::#ty #self_constr),
                }
            }
        }
    });
    let stores = input.variants.iter().map(|f| match f {
        EnumVariant::UnitEnum(StructUnit { ty, literal, .. }) => {
            quote! {
                Self::#ty => raw.copy_from_slice(&#literal)
            }
        }
        EnumVariant::EnumWithFields(sf) => {
            let ty = &sf.ty;
            let self_constr = sf.self_constr();
            let name = &sf.fields[0].name;
            quote! {
                Self::#ty #self_constr => #name.ser(raw)
            }
        }
    });

    let r = quote! {

        impl ::parsergen::HasWidth for #ty {
            const WIDTH: usize = #width;
        }

        impl ::parsergen::Parsergen<{#width}> for #ty {
            #[inline(always)]
            fn des<'a>(raw: &[u8; #width]) -> Option<Self> {
                #(#checks)*
                None
            }

            #[inline]
            fn ser(&self, raw: &mut [u8; #width]) {
                match self {
                    #(#stores),*
                }
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(#ty_str)
                    .field(&::parsergen::ValidBytes::<Self, {#width}>::from(raw))
                .finish()
            }
        }
    };
    Ok(r)
}

fn for_unit_struct(input: StructUnit) -> Result<TokenStream> {
    let StructUnit { ty, literal, .. } = &input;
    let width = input.literal.len();
    let ty_str = ty.to_string();
    let r = quote! {

        impl ::parsergen::HasWidth for #ty {
            const WIDTH: usize = #width;
        }

        impl ::parsergen::Parsergen<{#width}> for #ty {
            #[inline(always)]
            fn des<'a>(raw: &[u8; #width]) -> Option<Self> {
                if raw != & #literal {
                    return None
                }
                Some(Self)
            }

            #[inline]
            fn ser(&self, raw: &mut [u8; #width]) {
                raw.copy_from_slice(&#literal);
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(#ty_str)
                    .field(&::parsergen::ValidBytes::<Self, {#width}>::from(raw))
                .finish()
            }
        }
    };
    Ok(r)
}

fn for_struct_with_fields(input: StructFields) -> Result<TokenStream> {
    let StructFields { ty, width, .. } = &input;
    let ty_str = ty.to_string();
    let offsets = input.offsets().collect::<Vec<_>>();

    let parse_fields = input.fields.iter().map(
        |SeqField {
             name,
             parser,
             range: (f, t),
             ..
         }| {
            quote! {
                let slice = ::parsergen::arrayref::array_ref!(raw, #f, #t - #f);
                let #name = #parser;
            }
        },
    );

    let encode_fields = input.fields.iter().map(
        |SeqField {
             name,
             encoder,
             range: (f, t),
             offset_assert,
             ..
         }| {
            quote! {
                #offset_assert;
                let var = #name;
                let slice = ::parsergen::arrayref::array_mut_ref!(raw, #f, #t - #f);
                #encoder;
            }
        },
    );

    let slice_fields = input.fields.iter().map(
        |SeqField {
             name,
             slicer,
             range: (f, t),
             ..
         }| {
            if input.named {
                let name = name.to_string();
                quote! { d.field(#name, &#slicer(&raw[#f..#t])); }
            } else {
                quote! { d.field(&#slicer(&raw[#f..#t])); }
            }
        },
    );

    let make_slicer = if input.named {
        quote!(f.debug_struct(#ty_str))
    } else {
        quote!(f.debug_tuple(#ty_str))
    };

    let self_constr = input.self_constr();
    let r = quote! {

        impl ::parsergen::HasWidth for #ty {
            const WIDTH: usize = #width;
        }

        impl ::parsergen::Parsergen<{#width}> for #ty {
            fn des(raw: &[u8; #width]) -> Option<Self> {
                const O_0: usize = 0;
                #(#offsets)*
                #(#parse_fields)*
                Some(Self #self_constr)

            }

            fn ser(&self, raw: &mut [u8; #width]) {
                const O_0: usize = 0;
                let Self #self_constr = self;
                #(#offsets)*
                #(#encode_fields)*
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                const O_0: usize = 0;
                #(#offsets)*
                let mut d = #make_slicer;
                #(#slice_fields)*
                d.finish()
            }
        }
    };
    Ok(r)
}

struct SelfConstr {
    named: bool,
    contents: TokenStream,
}

impl ToTokens for SelfConstr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.named {
            token::Brace::default().surround(tokens, |t| self.contents.to_tokens(t))
        } else {
            token::Paren::default().surround(tokens, |t| self.contents.to_tokens(t))
        }
    }
}

impl Parse for DecodeInput {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let mut err = match StructInput::parse(input) {
            Ok(ok) => return Ok(DecodeInput::Struct(ok)),
            Err(err) => err, // --todo!("{:?}", err),
        };
        match EnumInput::parse(input) {
            Ok(ok) => return Ok(DecodeInput::Enum(ok)),
            Err(err2) => err.combine(err2),
        }
        Err(err)
    }
}

impl Parse for StructInput {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let _vis = input.parse::<Visibility>()?;
        input.parse::<Token![struct]>()?;
        let ty = input.parse::<Ident>()?;
        let content;
        if input.peek(token::Brace) {
            let _ = braced!(content in input);
            let fields = content.parse_terminated(SField::parse_named)?;
            let width = Punctuated::<TokenStream, Token![+]>::from_iter(
                fields.iter().map(|f| f.width.clone()),
            );
            Ok(StructInput::Fields(StructFields {
                ty,
                named: true,
                fields: sequence_fields(fields).collect(),
                width: quote!(#width),
            }))
        } else if input.peek(token::Paren) {
            let _ = parenthesized!(content in input);

            let fields = content.parse_terminated(SField::parse_unnamed)?;
            let width = Punctuated::<TokenStream, Token![+]>::from_iter(
                fields.iter().map(|f| f.width.clone()),
            );
            let r = StructInput::Fields(StructFields {
                ty,
                fields: sequence_fields(fields).collect(),
                width: quote!(#width),
                named: false,
            });
            input.parse::<Token![;]>()?;
            Ok(r)
        } else if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;
            let mut kind = FieldKind::Inherited;
            for attr in attrs {
                if attr.path.is_ident("parsergen") {
                    for a in attr
                        .parse_args_with(Punctuated::<Annotation, Token![,]>::parse_terminated)?
                        .into_iter()
                    {
                        match a {
                            Annotation::Literal(l) => kind = FieldKind::Literal(l),
                            Annotation::Fixed(i) => kind = FieldKind::Fixed(i),
                            Annotation::ViaIso(t) => kind = FieldKind::Iso(t),
                            Annotation::Offset(_) => {
                                return Err(Error::new(
                                    input.span(),
                                    "offset can be only used on named or unnamed fields",
                                ))
                            }
                        }
                    }
                }
            }
            match kind {
                FieldKind::Literal(literal) => {
                    let w = literal.len();

                    Ok(StructInput::Unit(StructUnit {
                        ty,
                        literal,
                        width: quote!(#w),
                    }))
                }
                _ => Err(Error::new(
                    input.span(),
                    "Unit struct must use 'literal' or 'hex' annotation",
                )),
            }
        } else {
            Err(Error::new(input.span(), "Unrecognized struct"))
        }
    }
}

#[derive(Debug)]
enum StructInput {
    Fields(StructFields),
    Unit(StructUnit),
}

#[derive(Debug)]
struct StructUnit {
    ty: Ident,
    literal: Literal,
    width: TokenStream,
}

#[derive(Debug)]
struct StructFields {
    ty: Ident,
    named: bool,
    fields: Vec<SeqField>,
    width: TokenStream,
}

// Single field
#[derive(Debug)]
struct SField {
    name: Option<Ident>,
    ty: Type,
    width: TokenStream,
    parser: TokenStream,
    encoder: TokenStream,
    slicer: TokenStream,
    offset: Option<LitInt>,
}

#[derive(Debug)]
struct SeqField {
    /// field name or field variable name for unnamed structs
    name: Ident,
    /// field type
    ty: Type,
    /// field width
    width: TokenStream,
    /// field slice range given as a pair of O_x identifiers
    range: (OffsetName, OffsetName),
    parser: TokenStream,
    encoder: TokenStream,
    slicer: TokenStream,
    offset_assert: TokenStream,
}

fn sequence_fields(i: Punctuated<SField, Token![,]>) -> impl Iterator<Item = SeqField> {
    i.into_iter().enumerate().map(|(ix, field)| SeqField {
        name: match field.name {
            Some(n) => n,
            None => Ident::new(&format!("var_{}", ix), Span::call_site()),
        },
        ty: field.ty,
        width: field.width,
        range: (OffsetName(ix), OffsetName(ix + 1)),
        parser: field.parser,
        encoder: field.encoder,
        slicer: field.slicer,
        offset_assert: match field.offset {
            Some(expected) => {
                let actual = OffsetName(ix);
                quote! {
                    const CHECK: usize = (#expected == #actual) as usize - 1;
                }
            }
            None => TokenStream::new(),
        },
    })
}

#[derive(Debug)]
enum FieldKind {
    Literal(Literal),
    Fixed(usize),
    Iso(Box<Type>),
    Inherited,
}

#[derive(Debug)]
struct OffsetName(usize);

impl ToTokens for OffsetName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::new(&format!("O_{}", self.0), Span::call_site());
        ident.to_tokens(tokens);
    }
}

impl SField {
    fn parse_named(input: parse::ParseStream) -> Result<Self> {
        Self::parse(input, true)
    }

    fn parse_unnamed(input: parse::ParseStream) -> Result<Self> {
        Self::parse(input, false)
    }

    fn parse(input: parse::ParseStream, named: bool) -> Result<Self> {
        let mut kind = FieldKind::Inherited;
        let mut offset = None;
        for attr in input.call(Attribute::parse_outer)? {
            if attr.path.is_ident("parsergen") {
                for a in attr
                    .parse_args_with(Punctuated::<Annotation, Token![,]>::parse_terminated)?
                    .into_iter()
                {
                    match a {
                        Annotation::Literal(l) => kind = FieldKind::Literal(l),
                        Annotation::Fixed(i) => kind = FieldKind::Fixed(i),
                        Annotation::ViaIso(t) => kind = FieldKind::Iso(t),
                        Annotation::Offset(o) => offset = Some(o),
                    }
                }
            }
        }
        let _vis = input.parse::<Visibility>()?;
        let name = if named {
            let r = Some(input.parse::<Ident>()?);
            input.parse::<Token![:]>()?;
            r
        } else {
            None
        };
        let ty = input.parse::<Type>()?;
        let arr = match &ty {
            Type::Array(arr) => Some(arr),
            _ => None,
        };

        let width = match (&kind, arr) {
            (FieldKind::Literal(xs), None) => {
                let w = xs.len();
                quote!(#w)
            }
            (FieldKind::Literal(_), Some(_)) => {
                return Err(Error::new(
                    ty.span(),
                    "literal/hex can't be used with an array type",
                ))
            }
            (FieldKind::Fixed(width), None) => {
                quote!(#width)
            }
            (FieldKind::Fixed(width), Some(TypeArray { len, .. })) => {
                quote!(#width * #len)
            }
            (FieldKind::Iso(iso), None) => {
                quote!(<#iso as ::parsergen::HasWidth>::WIDTH)
            }
            (FieldKind::Iso(iso), Some(TypeArray { len, .. })) => {
                quote!(<#iso as ::parsergen::HasWidth>::WIDTH * #len)
            }
            (FieldKind::Inherited, None) => {
                quote!(<#ty as ::parsergen::HasWidth>::WIDTH)
            }
            (FieldKind::Inherited, Some(TypeArray { len, elem, .. })) => {
                quote!(<#elem as ::parsergen::HasWidth>::WIDTH * #len)
            }
        };

        let parser = match (&kind, arr) {
            (FieldKind::Literal(lit), None) => {
                quote!(::parsergen::parse_literal::<#ty>(&#lit, slice)?)
            }
            (FieldKind::Literal(_), Some(_)) => unreachable!(),

            (&FieldKind::Fixed(width), None) => {
                let style = crate::primitives::get_style(&ty)?;
                parse_fixed_inner(&ty, style, width)
            }
            (&FieldKind::Fixed(width), Some(TypeArray { len, elem, .. })) => {
                let style = crate::primitives::get_style(elem)?;
                parse_fixed_arr_inner(elem, style, width, len)
            }
            (FieldKind::Iso(iso), None) => {
                quote!(<#ty>::try_from(<#iso as ::parsergen::Parsergen<{#width}>>::des(slice)?).ok()?)
            }
            (FieldKind::Iso(iso), Some(TypeArray { len, elem, .. })) => {
                let fwidth = quote!(<#iso as ::parsergen::HasWidth>::WIDTH);
                quote!(::parsergen::des_iso_array::<#elem, #iso, {#fwidth * #len}, {#fwidth}, #len>(slice)?)
            }
            (FieldKind::Inherited, None) => {
                quote!(<#ty as ::parsergen::Parsergen<{#width}>>::des(slice)?)
            }
            (FieldKind::Inherited, Some(TypeArray { len, elem, .. })) => {
                let fwidth = quote!(<#elem as ::parsergen::HasWidth>::WIDTH);
                quote!(::parsergen::des_array::<#elem, {#fwidth * #len}, {#fwidth}, #len>(slice)?)
            }
        };

        let encoder = match (&kind, arr) {
            (FieldKind::Literal(lit), None) => {
                quote!(slice.clone_from_slice(&#lit))
            }
            (FieldKind::Literal(_), Some(_)) => unreachable!(),
            (FieldKind::Fixed(width), None) => {
                quote!(<::parsergen::FixedT::<#ty, #width>>::new(*var).ser(slice))
            }
            (FieldKind::Fixed(width), Some(TypeArray { len, elem, .. })) => {
                quote!(::parsergen::ser_fixed_array::<#elem, {#width * #len}, {#width}, #len>(*var, slice))
            }
            (FieldKind::Iso(iso), None) => {
                quote!(<#iso>::from(*var).ser(slice))
            }
            (FieldKind::Iso(iso), Some(TypeArray { len, elem, .. })) => {
                let fwidth = quote!(<#iso as ::parsergen::HasWidth>::WIDTH);
                quote!(::parsergen::ser_iso_array::<#elem, #iso, {#fwidth * #len}, {#fwidth}, #len>(*var, slice))
            }
            (FieldKind::Inherited, None) => {
                quote!(<#ty as ::parsergen::Parsergen<{#width}>>::ser(var, slice))
            }
            (FieldKind::Inherited, Some(TypeArray { len, elem, .. })) => {
                let fwidth = quote!(<#elem as ::parsergen::HasWidth>::WIDTH);
                quote!(::parsergen::ser_array::<#elem, {#fwidth * #len}, {#fwidth}, #len>(*var, slice))
            }
        };

        let slicer = match (&kind, arr) {
            (FieldKind::Literal(_), None) => {
                quote!(::parsergen::PrettyBytes)
            }
            (FieldKind::Fixed(_), None) => {
                let style = crate::primitives::get_style(&ty)?;
                match style {
                    primitives::Style::Signed(_) => {
                        quote!(::parsergen::ValidBytes::<::parsergen::Signed, {#width}>::from)
                    }
                    primitives::Style::Unsigned => {
                        quote!(::parsergen::ValidBytes::<::parsergen::Unsigned, {#width}>::from)
                    }
                }
            }
            (FieldKind::Literal(_) | FieldKind::Fixed(_), Some(TypeArray { len, .. })) => {
                quote!(::parsergen::PrettyArrBytes::<#len>::new)
            }
            (FieldKind::Iso(ty), None) => {
                quote!(::parsergen::Sliced::<#ty, {#width}>::from)
            }
            (FieldKind::Iso(ity), Some(TypeArray { .. })) => {
                let fwidth = quote!(<#ity as ::parsergen::HasWidth>::WIDTH);
                quote!((|slice|::parsergen::SlicedArr::<#ity, {#fwidth}>::from(slice)))
            }
            (FieldKind::Inherited, None) => {
                quote!(::parsergen::Sliced::<#ty, {#width}>::from)
            }
            (FieldKind::Inherited, Some(TypeArray { elem, .. })) => {
                let fwidth = quote!(<#elem as ::parsergen::HasWidth>::WIDTH);
                quote!((|slice|::parsergen::SlicedArr::<#elem, {#fwidth}>::from(slice)))
            }
        };
        Ok(Self {
            name,
            slicer,
            ty,
            offset,
            width,
            parser,
            encoder,
        })
    }
}

impl StructFields {
    fn offsets(&'_ self) -> impl Iterator<Item = TokenStream> + '_ {
        self.fields
            .iter()
            .enumerate()
            .scan(quote!(0), |prev, (ix, f)| {
                let name = OffsetName(ix + 1);
                let width = &f.width;
                let r = quote! { const #name: usize = #prev + #width; };
                *prev = quote! { #name };
                Some(r)
            })
    }

    fn self_constr(&self) -> SelfConstr {
        let vars = self.fields.iter().map(|f| &f.name);
        SelfConstr {
            named: self.named,
            contents: quote!( #(#vars,)*),
        }
    }
}

#[derive(Debug)]
enum DecodeInput {
    Struct(StructInput),
    Enum(EnumInput),
}

#[derive(Debug)]
struct EnumInput {
    ty: Ident,
    width: TokenStream,
    variants: Punctuated<EnumVariant, Token![,]>,
}

impl Parse for EnumInput {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let _attrs = input.call(Attribute::parse_outer)?;
        let _vis = input.parse::<Visibility>()?;
        input.parse::<Token![enum]>()?;
        let ty = input.parse::<Ident>()?;
        let content;
        let _ = braced!(content in input);
        let variants = Punctuated::<EnumVariant, Token![,]>::parse_terminated(&content)?;
        if variants.is_empty() {
            return Err(Error::new(input.span(), "Empty enums are not supported"));
        }
        let width = match &variants[0] {
            EnumVariant::UnitEnum(s) => s.width.clone(),
            EnumVariant::EnumWithFields(s) => s.width.clone(),
        };
        Ok(EnumInput {
            ty,
            variants,
            width,
        })
    }
}

#[derive(Debug)]
enum EnumVariant {
    UnitEnum(StructUnit),
    EnumWithFields(StructFields),
}

impl Parse for EnumVariant {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let mut kind = FieldKind::Inherited;
        for attr in input.call(Attribute::parse_outer)? {
            if attr.path.is_ident("parsergen") {
                for a in attr
                    .parse_args_with(Punctuated::<Annotation, Token![,]>::parse_terminated)?
                    .into_iter()
                {
                    match a {
                        Annotation::Literal(l) => kind = FieldKind::Literal(l),
                        Annotation::Fixed(i) => kind = FieldKind::Fixed(i),
                        Annotation::ViaIso(t) => kind = FieldKind::Iso(t),
                        Annotation::Offset(_) => {
                            return Err(Error::new(
                                attr.span(),
                                "offset makes no sense with enum fields",
                            ))
                        }
                    }
                }
            }
        }

        let name = input.parse::<Ident>()?;

        if input.peek(Token![,]) {
            let literal = match kind {
                FieldKind::Literal(lit) => lit,
                _ => {
                    return Err(Error::new(
                        name.span(),
                        "Unit enum field must have literal/hex annotation",
                    ))
                }
            };
            let w = literal.len();
            Ok(EnumVariant::UnitEnum(StructUnit {
                literal,
                ty: name,
                width: quote!( #w ),
            }))
        } else if input.peek(token::Paren) {
            let content;
            let _ = parenthesized!(content in input);

            let fields = content.parse_terminated(SField::parse_unnamed)?;
            let width = Punctuated::<TokenStream, Token![+]>::from_iter(
                fields.iter().map(|f| f.width.clone()),
            );
            let fields = StructFields {
                ty: name,
                fields: sequence_fields(fields).collect(),
                width: quote!(#width),
                named: false,
            };
            Ok(EnumVariant::EnumWithFields(fields))
        } else if input.peek(token::Brace) {
            let content;
            let _ = braced!(content in input);

            let fields = content.parse_terminated(SField::parse_named)?;
            let width = Punctuated::<TokenStream, Token![+]>::from_iter(
                fields.iter().map(|f| f.width.clone()),
            );

            let fields = StructFields {
                ty: name,
                fields: sequence_fields(fields).collect(),
                width: quote!(#width),
                named: true,
            };
            Ok(EnumVariant::EnumWithFields(fields))
        } else {
            todo!("PARSE THIS {:?}", input)
        }
    }
}
