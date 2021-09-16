use proc_macro2::{Span, TokenStream};
use quote::*;
use syn::{punctuated::Punctuated, spanned::Spanned, *};

#[proc_macro_derive(Parsergen, attributes(parsergen))]
pub fn pg_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let r = impl_macro(parse_macro_input!(input as DeriveInput))
        .unwrap_or_else(|err| err.to_compile_error())
        .into();
    r
}

fn impl_macro(ast: DeriveInput) -> Result<TokenStream> {
    match ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => impl_struct(ast.ident, true, named),
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(FieldsUnnamed { unnamed, .. }),
            ..
        }) => impl_struct(ast.ident, false, unnamed),
        Data::Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => impl_unit(ast.ident, &ast.attrs),
        Data::Enum(DataEnum { variants, .. }) => impl_enum(ast.ident, variants),
        Data::Union(_) => Err(Error::new(
            ast.span(),
            "unions are not supported by parsergen",
        )),
    }
}

/// used for hex literal annotation, #[parsergen(hex: "DEADBEEF")]
struct HexLiteral(Vec<u8>);
impl parse::Parse for HexLiteral {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let hex_digit = |i: u8| match i as char {
            '0'..='9' => Ok(i - '0' as u8),
            'A'..='F' => Ok(i - 'A' as u8 + 10),
            'a'..='f' => Ok(i - 'A' as u8 + 10),
            _ => Err(Error::new(input.span(), "invalid hex literal")),
        };
        let hex_char = |i: &[u8]| Ok(hex_digit(i[0])? * 16 + hex_digit(i[1])?);
        Ok(HexLiteral(
            input
                .parse::<LitStr>()?
                .value()
                .as_bytes()
                .chunks(2)
                .map(hex_char)
                .collect::<Result<Vec<u8>>>()?,
        ))
    }
}

/// used for literal ASCII annotations, #[parsergen(literal: "B6021")]
struct AsciiLiteral(Vec<u8>);
impl parse::Parse for AsciiLiteral {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(AsciiLiteral(
            input
                .parse::<LitStr>()?
                .value()
                .as_bytes()
                .iter()
                .copied()
                .collect::<Vec<u8>>(),
        ))
    }
}

#[derive(Debug)]
struct FieldInfo {
    /// total field width in bytes, for arrays it's field width multiplied by cnt
    width: TokenStream,
    /// span for error messages
    span: Span,
    /// variable name for the field, either taken from named struct
    /// or constructed from the index
    var: Ident,
    /// code for deserialization, should assign result to #var
    des: TokenStream,
    /// code for serialization, should take result from #var
    ser: TokenStream,
    /// code for raw field slicing
    slice: TokenStream,
}

#[derive(Debug)]
enum Annotation {
    Literal(Vec<u8>),
    Fixed(LitInt),
    ViaIso(Type),
}

impl parse::Parse for Annotation {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let tname = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        if tname == "decimal" {
            Ok(Annotation::Fixed(input.parse()?))
        } else if tname == "literal" {
            Ok(Annotation::Literal(input.parse::<AsciiLiteral>()?.0))
        } else if tname == "hex" {
            Ok(Annotation::Literal(input.parse::<HexLiteral>()?.0))
        } else if tname == "via" {
            Ok(Annotation::ViaIso(input.parse()?))
        } else {
            Err(Error::new(input.span(), "unexpected annotation"))
        }
    }
}

impl FieldInfo {
    fn parse((ix, input): (usize, Field)) -> Result<Self> {
        let field_type = input.ty.clone();
        let field_type_span = input.ty.span();
        let span = input.span();
        let arr = match &field_type {
            syn::Type::Array(arr) => Some((arr.elem.clone(), arr.len.clone())),
            _ => None,
        };

        let range = Ident::new(&format!("R{}", ix + 1), span);
        let raw = quote! { raw[#range] };
        let var = input
            .ident
            .clone()
            .unwrap_or_else(|| Ident::new(&format!("v{}", ix), input.span()));
        let slice_fn = |body| match &input.ident.as_ref().map(|s| s.to_string()) {
            Some(ident) => quote! { .field(#ident, #body) },
            None => quote! { .field(#body) },
        };
        if let Some(attr) = input
            .attrs
            .iter()
            .find(|attr| attr.path.is_ident("parsergen"))
        {
            let ann = attr.parse_args::<Annotation>()?;
            let attr_span = attr.span();
            match (arr, ann) {
                // #[parsergen(literal: "hello")]
                (None, Annotation::Literal(pat)) => {
                    let width = pat.len();
                    let mismatch = match &input.ident {
                        Some(name) => format!("Not a valid value for {}", name),
                        None => format!("Not a valid value for field #{}", ix),
                    };
                    Ok(FieldInfo {
                        width: quote! { #width },
                        des: quote! {
                            if &#raw != &[ #( #pat),*] {
                                return Err(::parsergen::Error {_msg: #mismatch, _payload: raw})
                            }
                            let #var = <#field_type>::default();
                        },
                        ser: quote! {
                            let _ = #var;
                            for (c, p) in #raw.iter_mut().zip([ #( #pat ),*]) {
                                *c = p
                            }
                        },
                        slice: slice_fn(quote! ( &::parsergen::PrettyBytes(& #raw))),
                        span,
                        var,
                    })
                }

                // #[parsergen(decimal: 5)]
                // foo: u32,
                (None, Annotation::Fixed(width)) => Ok(FieldInfo {
                    width: quote_spanned! { attr_span => #width },
                    des: quote_spanned! { attr_span => let #var: #field_type = <::parsergen::FixedT::<#field_type, #width>>::des(&#raw)?.0; },
                    ser: quote_spanned! { attr_span => <::parsergen::FixedT<#field_type, #width>>::new(*#var).ser(&mut #raw); },
                    slice: slice_fn(quote!( &::parsergen::PrettyBytes(& #raw))),
                    span,
                    var,
                }),

                // #[parsergen(via: Foo)]
                // foo: Bar
                (None, Annotation::ViaIso(iso)) => Ok(FieldInfo {
                    width: quote!( <#iso as ::parsergen::Parsergen>::WIDTH ),
                    des: quote!( let #var = <#field_type>::from(<#iso as ::parsergen::Parsergen>::des(&#raw)?); ),
                    ser: quote!( <#iso>::from(*#var).ser(&mut #raw); ),
                    slice: slice_fn(quote!( & <::parsergen::Sliced<#iso>>::from(&#raw))),
                    span,
                    var,
                }),

                // #[parsergen(via: Foo)]
                // foo: Bar
                (Some(_), Annotation::Literal(_)) => Err(Error::new(
                    input.span(),
                    "Array of literals are not supported, you can file a ticket with a legit use case",
                )),

                // #[parsergen(decimal: 5)]
                // foo: [u32, 5]
                (Some((ty, cnt)), Annotation::Fixed(width)) => Ok(FieldInfo {
                    width: quote_spanned! { attr_span => (#width * #cnt) },
                    des: quote_spanned! { attr_span => let #var: [#ty; #cnt] = <::parsergen::FixedArrT::<#ty, #width, #cnt>>::des(&#raw)?.0; },
                    ser: quote_spanned! { attr_span => <::parsergen::FixedArrT<#ty, #width, #cnt>>::new(*#var).ser(&mut #raw); },
                    slice: slice_fn(quote!( &::parsergen::PrettyArrBytes(& #raw, #cnt))),
                    span,
                    var,
                }),

                // #[parsergen(via: #Foo)]
                // foo: [Bar; 5]
                (Some((ty, cnt)), Annotation::ViaIso(iso)) => Ok(FieldInfo{
                    width: quote!( <#iso as ::parsergen::Parsergen>::WIDTH * #cnt),
                    des: quote!( let #var: [#ty; #cnt] = <::parsergen::FixedArr::<#ty, #iso, #cnt>>::des(&#raw)?.0;),
                    ser: quote!( <::parsergen::FixedArr<#ty, #iso, #cnt>>::from(*var).ser(&mut #raw);),
                    slice: slice_fn(quote!( &<::parsergen::Sliced<::parsergen::FixedArr<#ty, #iso, #cnt>>>::from(&#raw))),
                    var,
                    span,
                })

            }
        } else {
            Ok(FieldInfo {
                width: quote_spanned! {field_type_span =>
                <#field_type as ::parsergen::Parsergen>::WIDTH },
                span,
                des: quote_spanned! { span => let #var = <#field_type as ::parsergen::Parsergen>::des(&#raw)?; },
                ser: quote_spanned! { span => <#field_type as ::parsergen::Parsergen>::ser(#var, &mut #raw); },
                slice: slice_fn(quote!( &<::parsergen::Sliced<#field_type>>::from(& #raw))),
                var,
            })
        }
    }
}

impl FieldInfo {
    fn parse_variant(variant: Variant) -> Result<Self> {
        if let Fields::Unit = &variant.fields {
            if let Some(attr) = variant
                .attrs
                .iter()
                .find(|attr| attr.path.is_ident("parsergen"))
            {
                let ann = attr.parse_args::<Annotation>()?;
                let var = variant.ident.clone();
                let span = variant.span();
                match ann {
                    Annotation::Literal(pat) => {
                        let width = pat.len();
                        Ok(FieldInfo {
                            width: quote!(#width),
                            des: quote! {
                                if &raw == &[ #( #pat ),*] {
                                    return Ok(Self::#var)
                                }
                            },
                            ser: quote! { Self::#var => {
                                for (r, c) in raw.iter_mut().zip([#( #pat ),*].iter()) {
                                    *r = *c;
                                }
                            }},
                            slice: quote!(),
                            var,
                            span,
                        })
                    }
                    Annotation::Fixed(_) | Annotation::ViaIso(_) => Err(Error::new(
                        variant.span(),
                        "Only 'literal' or 'hex' annotations in enums are supported",
                    )),
                }
            } else {
                Err(Error::new(
                    variant.span(),
                    "unit enum variant must be annotated with 'literal' or 'hex'",
                ))
            }
        } else if let Fields::Unnamed(un) = variant.fields {
            let fields = un.unnamed;
            if fields.len() != 1 {
                let msg = "Only unit or single unnamed field variants are supported";
                return Err(Error::new(fields.span(), msg));
            };
            let var = variant.ident;
            Ok(FieldInfo {
                width: quote!(),
                span: fields.span(),
                des: quote! {
                    if let Ok(res) = <#fields as ::parsergen::Parsergen>::des(raw) {
                        return Ok(Self::#var(res))
                    }
                },
                ser: quote! { Self::#var(x) => x.ser(raw) },
                slice: quote!(),
                var,
            })
        } else {
            let msg = "Only unit or single unnamed field variants are supported";
            return Err(Error::new(variant.span(), msg));
        }
    }
}

fn impl_unit(name: Ident, attrs: &[Attribute]) -> Result<TokenStream> {
    let pat = if let Some(attr) = attrs.iter().find(|attr| attr.path.is_ident("parsergen")) {
        let ann = attr.parse_args::<Annotation>()?;
        match ann {
            Annotation::Literal(pat) => pat,
            Annotation::Fixed(_) | Annotation::ViaIso(_) => {
                return Err(Error::new(
                    name.span(),
                    "#[parsergen] hex or literal annotation required",
                ))
            }
        }
    } else {
        return Err(Error::new(
            name.span(),
            "#[parsergen] hex or literal annotation required",
        ));
    };
    let width = pat.len();
    let name_str = name.to_string();
    let mismatch = format!("Not a valid value for {}", name);
    Ok(quote! {
        impl ::parsergen::Parsergen for #name {
            const WIDTH: usize = #width;
            #[inline]
            fn des<'a>(raw: &[u8]) -> ::parsergen::Result<#name> {
                if &raw != &[ #( #pat),*] {
                    return Err(::parsergen::Error {_msg: #mismatch, _payload: raw})
                }
                Ok(#name)
            }

            #[inline]
            fn ser(&self, raw: &mut [u8]) {
                for (c, p) in raw.iter_mut().zip([ #( #pat ),*]) {
                    *c = p
                }
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(#name_str)
                    .field(&::parsergen::PrettyBytes(raw))
                .finish()
            }
        }
    })
}

fn impl_enum(name: Ident, variants: Punctuated<Variant, Token![,]>) -> Result<TokenStream> {
    let infos = variants
        .into_iter()
        .map(FieldInfo::parse_variant)
        .collect::<Result<Vec<_>>>()?;
    let name_str = name.to_string();

    let width = &infos[0].width;

    let checks = infos.iter().map(|fi| &fi.des);
    let stores = infos.iter().map(|fi| &fi.ser);
    let mismatch = format!("Invalid {}", name);
    Ok(quote! {
        impl ::parsergen::Parsergen for #name {
            const WIDTH: usize = #width;

            #[inline]
            fn des<'a>(raw: &[u8]) -> ::parsergen::Result<#name> {
                #(#checks)*
                Err(::parsergen::Error{_msg: #mismatch, _payload: raw })
            }

            #[inline]
            fn ser(&self, raw: &mut [u8]) {
                match self {
                    #(#stores),*
                }
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(#name_str)
                    .field(&::parsergen::PrettyBytes(raw))
                .finish()
            }
        }
    })
}

fn impl_struct(
    name: Ident,
    named: bool,
    fields: Punctuated<Field, Token![,]>,
) -> syn::Result<TokenStream> {
    let name_span = fields.span();

    let infos = fields
        .into_iter()
        .enumerate()
        .map(FieldInfo::parse)
        .collect::<Result<Vec<_>>>()?;

    let width = infos
        .iter()
        .map(|info| info.width.clone())
        .collect::<Punctuated<_, Token![+]>>();

    let field_ranges = infos
        .iter()
        .enumerate()
        .map(|(ix, info)| {
            let field_span = info.span;
            let this = Ident::new(&format!("R{}", ix + 1), field_span);
            let prev = Ident::new(&format!("R{}", ix), field_span);
            let this_width = &info.width;
            quote_spanned! { field_span =>
                const #this: std::ops::Range<usize> = #prev.end..#prev.end + #this_width;
            }
        })
        .collect::<Vec<_>>();

    let ranges = quote_spanned! { name_span =>
        const R0: std::ops::Range<usize> = 0..0;
        #( #field_ranges )*
    };

    let vars = infos.iter().map(|info| &info.var).collect::<Vec<_>>();

    let produce_result = if named {
        quote! { Self { #(#vars),* } }
    } else {
        quote! { Self ( #(#vars),* ) }
    };

    let split_parameter = if named {
        quote! { let Self { #(#vars),* } = &self; }
    } else {
        quote! { let Self ( #(#vars),* ) = &self; }
    };

    let parse_fields = infos.iter().map(|info| &info.des);
    let encode_fields = infos.iter().map(|info| &info.ser);
    let slice_fields = infos.iter().map(|info| &info.slice);

    let name_str = name.to_string();
    let slicer = if named {
        quote_spanned! { name_span => f.debug_struct(#name_str) }
    } else {
        quote_spanned! { name_span => f.debug_tuple(#name_str) }
    };
    Ok(quote_spanned! {name_span =>
        impl ::parsergen::Parsergen for #name {
            const WIDTH: usize = #width;

            #[inline]
            fn des<'a>(raw: &[u8]) -> ::parsergen::Result<#name> {
                #ranges
                #(#parse_fields)*
                Ok(#produce_result)
            }

            #[inline]
            fn ser(&self, raw: &mut [u8]) {
                #ranges
                #split_parameter
                #(#encode_fields)*
            }

            fn slice(raw: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #ranges
                #slicer
                #(#slice_fields)*
                .finish()
            }
        }
    })
}
