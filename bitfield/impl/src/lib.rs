use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Fields, FieldsNamed, Ident, ItemEnum, ItemStruct, Meta, MetaNameValue};

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemStruct);

    let mut size = quote! { 0 };
    let mut getters_and_setters = quote! {};
    let mut bits_attr_checks = quote! {};
    match item.fields {
        Fields::Named(FieldsNamed { named: fields, .. }) => {
            for field in fields {
                let ty = field.ty;
                // Generate check expression if the filed has attribute `#[bits = ..]`
                for attr in field.attrs.iter() {
                    if let Meta::NameValue(MetaNameValue { ref path, ref value, .. }) = attr.meta {
                        if path.is_ident("bits") {
                            bits_attr_checks.extend(quote_spanned! { value.span() =>
                                // const _: ::bitfield::checks::CorrectBitfieldWidth<{ #value == #ty::BITS }> = ();
                                const _: [(); #value] = [(); #ty::BITS];
                            });
                        }
                    }
                }

                // Here comes for size calculation. Since I cannot use `Sepecifier` trait
                // outside this crate, I generate an expression like:
                // 0 #( + #ty::BITS )* (or `0 #( + <#ty as Specifier>::BITS )*`)
                // This expression will be expanded to a constant expression before Rust
                // compiling, the compiler knows the value of it, and it's OK to compile.
                // Save start and end point of this field for generating getter and setter
                let start = size.clone();
                size.extend(quote! { + #ty::BITS });
                let end = size.clone();

                // Getters and setters
                let ident = field.ident.unwrap();
                let getter_ident = Ident::new(&format!("get_{}", ident), ident.span());
                let setter_ident = Ident::new(&format!("set_{}", ident), ident.span());
                let access_type = quote! { <#ty as ::bitfield::Specifier>::Access };
                let raw_type = quote! { <#ty as ::bitfield::Specifier>::Raw };

                let getter_and_setter = quote! {
                    pub fn #getter_ident(&self) -> #access_type {
                        let (start, end) = (#start, #end - 1);
                        let (start_index, end_index) = (start / 8, end / 8);
                        
                        // Use u128 to avoid overflow
                        let mut output = [0u8; 16];
                        let (mut output_index, mut travesal_index) = (0, start_index);
                        while travesal_index <= end_index {
                            output[output_index] = self.data[travesal_index];

                            output_index += 1;
                            travesal_index += 1;
                        }

                        let mask = ((1 << (end - start + 1)) - 1) << start;
                        // No No No
                        // Is it the only way?
                        // unsafe {
                        //     ::std::mem::transmute(
                        //         (((u128::from_le_bytes(output) << (start - (start % 8))) & mask) >> start) as #raw_type
                        //     )
                        // }
                        // Thanks for Occar421's solution
                        <#ty as Specifier>::to_access(
                            (((u128::from_le_bytes(output) << (start - (start % 8))) & mask) >> start) as #raw_type
                        )
                    }

                    pub fn #setter_ident(&mut self, value: #access_type) {
                        let (start, end) = (#start, #end - 1);
                        let (start_index, end_index) = (start / 8, end / 8);
                        let (start_offset, end_offset) = (start % 8, end % 8);

                        let mut input = u128::to_le_bytes((value as u128) << start_offset);
                        let mask = (((1u16 << (8 - start_offset)) - 1) << start_offset) as u8;
                        input[0] = (self.data[start_index] & !mask) | (input[0] & mask);
                        let mask = ((1u16 << (end_offset + 1)) - 1) as u8;
                        let last = end_index - start_index;
                        input[last] = (self.data[end_index] & !mask) | (input[last] & mask);

                        let (mut input_index, mut travesal_index) = (0, start_index);
                        while travesal_index <= end_index {
                            self.data[travesal_index] = input[input_index];

                            input_index += 1;
                            travesal_index += 1;
                        }
                    }
                };
                getters_and_setters.extend(getter_and_setter);
            }
        },
        _ => return syn::Error::new(
                Span::call_site(),
                "`#[bitfield]` only supports named structs",
            ).into_compile_error().into(),
    }

    // Integration of code
    let ItemStruct { attrs, vis, ident, generics, .. } = item;
    let output = quote! {
        #[repr(C)]
        #(#attrs)*
        #vis struct #ident #generics {
            data: [u8; (#size) / 8],
        }

        impl #ident {
            pub fn new() -> Self {
                Self {
                    data: [0u8; (#size) / 8],
                }
            }

            #getters_and_setters
        }

        // const _: () = <::bitfield::checks::Remainder::<{ (#size) % 8 }> as ::bitfield::checks::TotalSizeIsMultipleOfEightBits>::check();
        const _: ::bitfield::checks::MultipleOf8Bits::<{ (#size) % 8 }> = ();
        #bits_attr_checks
    };
    eprintln!("{:#}", output.to_string());

    output.into()
}

#[proc_macro]
pub fn define_and_specify_types(_input: TokenStream) -> TokenStream {
    let mut output = quote! {};
    for i in 1..=64usize {
        let ident = Ident::new(&format!("B{}", i), Span::call_site());
        let ty = Ident::new(type_from_bits(i), Span::call_site());

        let specify = quote! {
            pub enum #ident {}

            impl Specifier for #ident {
                const BITS: usize = #i;
                type Access = #ty;
                type Raw = #ty;

                fn to_access(raw: Self::Raw) -> Self::Access {
                    raw
                }
            }
        };

        output.extend(specify);
    }

    output.into()
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn bitfield_specifier(input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemEnum);
    let mut output = quote! {};
    let ident = item.ident;
    let variants_count = item.variants.len();

    // if variants_count.is_power_of_two() {
    //     output.extend(quote! {
    //         impl ::bitfield::checks::PowerOf2 for #ident {
    //             type Check = ();
    //         }
    //     });
    // }
    if !variants_count.is_power_of_two() {
        return syn::Error::new(Span::call_site(), "BitfieldSpecifier expected a number of variants which is a power of 2")
            .into_compile_error()
            .into();
    }

    let bits = (variants_count as f32).log2() as usize;
    let raw_type = Ident::new(type_from_bits(bits), Span::call_site());
    let variant_idents: Vec<_> = item.variants.iter().map(|variant| &variant.ident).collect();
    output.extend(quote! {
        impl ::bitfield::Specifier for #ident {
            const BITS: usize = #bits;
            type Access = #ident;
            type Raw = #raw_type;

            fn to_access(raw: Self::Raw) -> Self::Access {
                match raw {
                    #(_ if #ident::#variant_idents as Self::Raw == raw => #ident::#variant_idents,)*
                    _ => unreachable!(),
                }
            }
        }

        // const _: <#ident as ::bitfield::checks::PowerOf2>::Check = ();
    });

    // Check if the values of discriminants are in range
    if item.variants.iter().any(|variant| if let Some(_) = variant.discriminant { true } else { false }) {
        for variant in item.variants.iter() {
            let variant_ident = &variant.ident;
            let variant_path = quote! { #ident::#variant_ident };
            let span = variant.span();
            output.extend(quote_spanned! { span =>
                const _: ::bitfield::checks::DiscriminantIsValid<{ ((#variant_path as i128) >= 0) && ((#variant_path as i128) < (1 << #bits)) }>
                    = ();
            });
        }
    }

    output.into()
}

fn type_from_bits(bits: usize) -> &'static str {
    match bits {
        1..=8 => "u8",
        1..=16 => "u16",
        1..=32 => "u32",
        1..=64 => "u64",
        _ => unreachable!(),
    }
}