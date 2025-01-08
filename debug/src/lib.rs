use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, parse_str, punctuated::Punctuated, spanned::Spanned, token::Comma, AngleBracketedGenericArguments, Attribute, Data, DeriveInput, Expr, ExprLit, Field, Fields, GenericArgument, GenericParam, Generics, Ident, Lit, LitStr, Meta, PathArguments, PathSegment, Type, TypePath
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match expand(input) {
        Ok(token_stream) => token_stream,
        Err(err) => err.into_compile_error(),
    }
    .into()
}

fn expand(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let ident = input.ident;
    let ident_lit = ident.to_string();
    let fields = match input.data {
        Data::Struct(data_struct) => match data_struct.fields {
            Fields::Named(fields) => fields.named,
            _ => {
                return Err(syn::Error::new(
                    Span::call_site(),
                    "unnamed struct not supported",
                ))
            }
        },
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "this macro can only be derived on structs",
            ))
        }
    };

    let mut generics = input.generics;
    let qualified_bound_on_struct = parse_nested_meta_value(&input.attrs)?;
    let mut qualified_bounds_on_fields = vec![];
    for field in fields.iter() {
        qualified_bounds_on_fields.push(parse_nested_meta_value(&field.attrs)?);
    }
    let mut excluded_generics = get_phantom_generics(&fields);

    fn add_bound(generics: Generics, bound: String, exclude: &mut Vec<Ident>) -> syn::Result<Generics> {
        if !bound.is_empty() {
            let capital = bound.chars().next().unwrap().to_string();
            let type_param_ident = Ident::new(&capital, Span::call_site());
            if !exclude.contains(&type_param_ident) {
                exclude.push(type_param_ident);
            }
            add_qualified_bound(generics, bound)
        } else {
            Ok(generics)
        }
    }
    if !qualified_bounds_on_fields.is_empty() {
        for bound in qualified_bounds_on_fields {
            generics = add_bound(generics, bound, &mut excluded_generics)?;
        }
    }
    if !qualified_bound_on_struct.is_empty() {
        generics = add_bound(generics, qualified_bound_on_struct, &mut excluded_generics)?;
    }

    let generics = add_trait_bounds_exclude(generics, &fields, excluded_generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let field_calls = add_fields_to_build(&fields);

    let expanded = quote! {
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                fmt.debug_struct(#ident_lit)
                    #(#field_calls)*
                    .finish()
            }
        }
    };

    Ok(expanded)
}

fn add_fields_to_build(fields: &Punctuated<Field, Comma>) -> Vec<proc_macro2::TokenStream> {
    fields
        .iter()
        .map(|f| {
            let ident = f.ident.clone().unwrap();
            let ident_lit = ident.to_string();

            match parse_attribute_name_value(&f.attrs) {
                Err(err) => err.into_compile_error(),
                Ok(format) => {
                    if format.is_empty() {
                        quote! {
                            .field(#ident_lit, &self.#ident)
                        }
                    } else {
                        quote! {
                            .field(#ident_lit, &::std::format_args!(#format, &self.#ident))
                        }
                    }
                }
            }
        })
        .collect()
}

fn parse_attribute_name_value(attributes: &Vec<Attribute>) -> syn::Result<String> {
    let mut value = String::new();
    let debug_attr = attributes.iter().find(|attr| attr.path().is_ident("debug"));
    if let Some(attr) = debug_attr {
        if let Meta::NameValue(name_value) = &attr.meta {
            let expr = &name_value.value;
            value = match expr {
                Expr::Lit(ExprLit { lit, .. }) => match lit {
                    Lit::Str(s) => s,
                    _ => return Err(syn::Error::new(expr.span(), r#"expect a str like "foo""#)),
                },
                _ => return Err(syn::Error::new(expr.span(), "expect a literal expression")),
            }
            .value();
        }
    }

    Ok(value)
}

fn add_trait_bounds_exclude(
    mut generics: Generics,
    refer: &Punctuated<Field, Comma>,
    exclude: Vec<Ident>,
) -> Generics {
    let mut generics_idents = vec![];
    for param in &generics.params {
        if let GenericParam::Type(type_param) = param {
            generics_idents.push(type_param.ident.clone());
        }
    }
    let generics_idents = generics_idents;

    let mut exclude = exclude;
    for field in refer {
        match &field.ty {
            Type::Path(TypePath { path, .. }) => match &path.segments.first().unwrap().arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                    match args.first().unwrap() {
                        GenericArgument::Type(Type::Path(TypePath { path, .. })) => {
                            let segments = &path.segments;
                            let ident = &segments.first().unwrap().ident;
                            if generics_idents.contains(ident) && segments.len() > 1 {
                                generics
                                    .make_where_clause()
                                    .predicates
                                    .push(parse_quote!(#segments: ::std::fmt::Debug));
                                exclude.push(ident.clone());
                            }
                        }
                        _ => continue,
                    }
                }
                _ => continue,
            },
            _ => continue,
        }
    }

    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = param {
            if !exclude.contains(&&type_param.ident) {
                type_param.bounds.push(parse_quote!(::std::fmt::Debug));
            }
        }
    }

    generics
}

fn get_phantom_generics(fields: &Punctuated<Field, Comma>) -> Vec<Ident> {
    let mut phantom_generics = vec![];

    for field in fields {
        match &field.ty {
            Type::Path(TypePath { path, .. }) => {
                for segment in &path.segments {
                    if segment.ident.eq("PhantomData") {
                        match &segment.arguments {
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                args,
                                ..
                            }) => match args.first().unwrap() {
                                GenericArgument::Type(Type::Path(TypePath { path, .. })) => {
                                    match path.segments.first() {
                                        Some(PathSegment { ident, .. }) => {
                                            phantom_generics.push(ident.clone());
                                        }
                                        _ => continue,
                                    }
                                }
                                _ => continue,
                            },
                            _ => continue,
                        }
                    }
                }
            }
            _ => continue,
        }
    }

    phantom_generics
}

fn add_qualified_bound(mut generics: Generics, bound: String) -> syn::Result<Generics> {
    let where_clause = parse_str(&bound)?;
    generics.make_where_clause().predicates.push(where_clause);

    Ok(generics)
}

fn parse_nested_meta_value(attributes: &Vec<Attribute>) -> syn::Result<String> {
    let mut bound = String::new();
    for attr in attributes {
        if attr.path().is_ident("debug") {
            if let Meta::List(_) = attr.meta {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("bound") {
                        let s: LitStr = meta.value()?.parse()?;
                        bound = s.value();
                    }
    
                    Ok(())
                })?;
            }
        }
    }

    Ok(bound)
}
