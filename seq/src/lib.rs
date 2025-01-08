use std::ops::Range;

use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use quote::{quote, ToTokens};
use syn::{braced, parse::{Parse, ParseStream}, parse2, parse_macro_input, Ident, LitInt, Token};

// I could't figure out how to make this project easier with `syn::parse`.
// So I generated a new TokenStream by traversing and manipulating the token trees
// directly at a low level, using only `proc_macro2` crate while parsing the 
// body block of `seq` macro, which is the best solution I could come up with.
// I really tried my best. Maybe it's the expected solution?
// Now I'd like to move on to the `sorted`project.
#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SeqInput);

    match expand(input) {
        Ok(token_stream) => token_stream,
        Err(err) => err.into_compile_error(),
    }.into()
}

struct SeqInput {
    decleared_marker: Ident,
    start: usize,
    end: usize,
    body: proc_macro2::TokenStream,
}

impl Parse for SeqInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let decleared_marker = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![..]>()?;
        let end = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<LitInt>()?.base10_parse::<usize>()? + 1
        } else {
            input.parse::<LitInt>()?.base10_parse()?
        };
        let content;
        braced!(content in input);
        let body = content.parse()?;

        Ok(Self {
            decleared_marker,
            start,
            end,
            body,
        })
    }
}

fn expand(input: SeqInput) -> syn::Result<proc_macro2::TokenStream> {
    let SeqInput { decleared_marker, start, end, body } = input;

    let (maybe_new_body, has_expression) = transform_expression(body.clone(), &decleared_marker, start..end)?;
    if !has_expression {
        let mut new_body = proc_macro2::TokenStream::new();
        for i in start..end {
            new_body.extend(modify_ident_to(body.clone(), &decleared_marker, i)?);
        }
        Ok(new_body)
    } else {
        Ok(maybe_new_body)
    }
}

fn transform_expression(
    stream: proc_macro2::TokenStream,
    marker: &Ident, range: Range<usize>
) -> syn::Result<(proc_macro2::TokenStream, bool)> {
    let stream = stream.into_iter().collect::<Vec<_>>();
    let mut output = proc_macro2::TokenStream::new();
    let mut sign = false;

    let mut i = 0;
    while (0..stream.len()).contains(&i) {
        let tree = stream[i].clone();
        match tree {
            TokenTree::Punct(punct) => {
                if punct.as_char() == '#' {
                    if let Some(TokenTree::Group(group)) = stream.get(i + 1) {
                        if group.delimiter() == Delimiter::Parenthesis {
                            if let Some(TokenTree::Punct(punct)) = stream.get(i + 2) {
                                if punct.as_char() == '*' {
                                    let mut transformed = proc_macro2::TokenStream::new();
                                    for j in range.clone() {
                                        transformed.extend(modify_ident_to(group.stream(), marker, j)?);
                                    }
                                    sign = true;

                                    i += 3;
                                    output.extend(transformed);
                                    continue;
                                }
                            }
                        }
                    }
                }

                i += 1;
                output.extend(quote! {#punct});
            },
            TokenTree::Group(group) => {
                let span = group.span();
                let (stream, has_repeated_expression) = transform_expression(group.stream(), marker, range.clone())?;
                sign |= has_repeated_expression;
                let mut new_group = Group::new(group.delimiter(), stream);
                new_group.set_span(span);

                i += 1;
                output.extend(quote! {#new_group});
            },
            TokenTree::Ident(ident) => {
                i += 1;
                output.extend(quote! {#ident});
            },
            TokenTree::Literal(literal) => {
                i += 1;
                output.extend(quote! {#literal})
            },
        }
    }

    Ok((output, sign))
}

fn modify_ident_to(stream: proc_macro2::TokenStream, from_marker: &Ident, to: usize) -> syn::Result<proc_macro2::TokenStream> {
    let stream = stream.into_iter().collect::<Vec<_>>();

    let mut output = proc_macro2::TokenStream::new();
    let mut i = 0;
    while (0..stream.len()).contains(&i) {
        let tree = stream[i].clone();
        match tree {
            TokenTree::Ident(prefix) => {
                if let Some(TokenTree::Punct(punct)) = stream.get(i + 1) {
                    if punct.as_char() == '~' {
                        if let Some(TokenTree::Ident(marker_met)) = stream.get(i + 2) {
                            if !marker_met.eq(from_marker) {
                                return Err(syn::Error::new(marker_met.span(), format!("undecleared marker, expected {}", from_marker)));
                            }
                            let ident = Ident::new(&format!("{}{}", prefix, to), prefix.span());

                            i += 3;
                            output.extend(quote! {#ident});
                            continue;
                        }
                    }
                }

                let ident = prefix;
                if ident.eq(from_marker) {
                    let replacement = Literal::usize_unsuffixed(to);
                    output.extend(quote! {#replacement});
                } else {
                    output.extend(quote! {#ident});
                }
                i += 1;
            },
            TokenTree::Group(group) => {
                let span = group.span();
                let mut new_group = Group::new(group.delimiter(), modify_ident_to(group.stream(), from_marker, to)?);
                new_group.set_span(span);
                i += 1;
                output.extend(quote! {#new_group});
            },
            TokenTree::Punct(punct) => {
                i += 1;
                output.extend(quote! {#punct});
            },
            TokenTree::Literal(literal) => {
                i += 1;
                output.extend(quote! {#literal})
            },
        }
    }

    Ok(output)
}
