use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Error, Field, Fields, GenericArgument, Ident, LitStr, PathArguments, PathSegment, Type
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match input.data {
        Data::Struct(_) => {},
        _ => {
            return Error::new(Span::call_site(), "the macro must be derived on struct")
                .into_compile_error()
                .into();
        },
    }

    let context = Context::from(&input);

    let expanded_impl = expand_impl_block(&context);
    let builder = get_builder(&context);

    let expanded = quote! {
        #expanded_impl

        #builder
    };

    expanded.into()
}

struct Context {
    pub ident: Ident,
    pub builder_ident: Ident,
    pub fields: Vec<FieldInfo>,
}

impl From<&DeriveInput> for Context {
    fn from(value: &DeriveInput) -> Self {
        Self {
            ident: value.ident.to_owned(),
            builder_ident: Self::generate_ident(&value.ident),
            fields: Self::unpack_fields(value),
        }
    }
}

impl Context {
    fn generate_ident(ident: &Ident) -> Ident {
        Ident::new(&format!("{}Builder", ident), ident.span())
    }

    fn unpack_fields(input: &DeriveInput) -> Vec<FieldInfo> {
        let fields = match &input.data {
            Data::Struct(s) => match &s.fields {
                Fields::Named(f) => &f.named,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        fields.iter().map(|f| FieldInfo::from(f)).collect()
    }
}

struct FieldInfo {
    pub ident: Ident,
    pub attribute_parse_result: syn::Result<Option<Ident>>,
    pub ty: Type,
    pub is_option: bool,
}

impl From<&Field> for FieldInfo {
    fn from(value: &Field) -> Self {
        Self {
            ident: value.ident.to_owned().unwrap(),
            attribute_parse_result: Self::parse_for_attribute_value(value),
            ty: value.ty.to_owned(),
            is_option: Self::is_path_type(&value.ty, "Option"),
        }
    }
}

impl FieldInfo {
    fn parse_for_attribute_value(f: &Field) -> syn::Result<Option<Ident>> {
        let mut default_ident = f.ident.clone().unwrap();
        let builder_attr = f
            .attrs
            .iter()
            .filter(|attr| attr.meta.path().is_ident("builder"))
            .next();

        match builder_attr {
            Some(attr) => {
                if !Self::is_path_type(&f.ty, "Vec") {
                    return Err(Error::new(
                        f.ty.span(),
                        "mismatched type, expected `Vec`",
                    ));
                }

                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("each") {
                        let val = meta.value()?;
                        let s: LitStr = val.parse()?;
                        default_ident = Ident::new(&s.value(), s.span());

                        Ok(())
                    } else {
                        Err(Error::new_spanned(
                            attr.meta.require_list().unwrap(),
                            r#"expected `builder(each = "...")`"#,
                        ))
                    }
                })
                .map(|_| Some(default_ident))
            }
            None => Ok(None),
        }
    }

    fn get_path_segments(ty: &Type) -> Option<&PathSegment> {
        match ty {
            Type::Path(p) => p.path.segments.iter().peekable().peek().map(|seg| *seg),
            _ => None,
        }
    }

    fn is_path_type(ty: &Type, expected: &str) -> bool {
        if let Some(seg) = Self::get_path_segments(&ty) {
            seg.ident.eq(&Ident::new(expected, ty.span()))
        } else {
            false
        }
    }

    pub fn get_generic_type(&self) -> Option<&Type> {
        if let Some(seg) = Self::get_path_segments(&self.ty) {
            match seg.arguments {
                PathArguments::AngleBracketed(ref inner) => match inner.args.iter().next().unwrap()
                {
                    GenericArgument::Type(t) => Some(t),
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn is_vec_builder(&self) -> bool {
        self.attribute_parse_result
            .clone()
            .is_ok_and(|f| f.is_some())
    }
}

fn get_builder(context: &Context) -> proc_macro2::TokenStream {
    let builder_ident = &context.builder_ident;

    let builder_struct = get_builder_struct(context);
    let setters = get_setters(context);
    let build_func = get_build_function(context);

    quote! {
        #builder_struct

        impl #builder_ident {
            #(#setters)*

            #build_func
        }
    }
}

fn get_builder_struct(context: &Context) -> proc_macro2::TokenStream {
    let builder_ident = &context.builder_ident;
    let new_field_names = context.fields.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;

        if f.is_vec_builder() || f.is_option {
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: ::std::option::Option<#ty>
            }
        }
    });

    quote! {
        pub struct #builder_ident {
            #(#new_field_names),*
        }
    }
}

fn get_setters(context: &Context) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    context.fields.iter().map(|f| {
        if let Err(err) = &f.attribute_parse_result {
            let compile_error = err.clone().into_compile_error();
            return quote! {#compile_error};
        }

        let ident = &f.ident;
        let method_ident = match f.attribute_parse_result.clone().unwrap() {
            Some(ident) => ident,
            None => f.ident.clone(),
        };
        let ty = if f.is_option || f.is_vec_builder() {
            f.get_generic_type().unwrap()
        } else {
            &f.ty
        };

        if f.is_vec_builder() {
            quote! {
                pub fn #method_ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident.push(#ident);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = ::std::option::Option::Some(#ident);
                    self
                }
            }
        }
    })
}

fn get_build_function(context: &Context) -> proc_macro2::TokenStream {
    let check_members_expr = context.fields.iter().map(|f| {
        let ident = &f.ident;
        let ident_string = f.ident.to_string();

        if !f.is_option && !f.is_vec_builder() {
            quote! {
                if self.#ident.is_none() {
                    return ::std::result::Result::Err(::std::boxed::Box::<dyn ::std::error::Error>::from(
                        ::std::format!("{} is not set", #ident_string)
                    ));
                }
            }
        } else {
            quote! {}
        }
    });

    let assignments = context.fields.iter().map(|f| {
        let ident = &f.ident;

        if f.is_option {
            quote! {
                #ident: self.#ident.take()
            }
        } else if f.is_vec_builder() {
            quote! {
                #ident: self.#ident.to_owned()
            }
        } else {
            quote! {
                #ident: self.#ident.take().unwrap()
            }
        }
    });

    let ident = &context.ident;

    quote! {
        pub fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
            #(#check_members_expr)*

            ::std::result::Result::Ok(#ident {
                #(#assignments),*
            })
        }
    }
}

fn expand_impl_block(context: &Context) -> proc_macro2::TokenStream {
    let ident = &context.ident;
    let builder_ident = &context.builder_ident;
    let fields_init = context.fields.iter().map(|f| {
        let ident = &f.ident;
        if f.is_vec_builder() {
            quote! {
                #ident: ::std::vec![]
            }
        } else {
            quote! {
                #ident: ::std::option::Option::None
            }
        }
    });

    quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#fields_init),*
                }
            }
        }
    }
}
