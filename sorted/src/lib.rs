use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, visit_mut::VisitMut, Arm, Expr, ExprMatch, Item, ItemFn, Meta, Pat, PatIdent, PatLit, PatPath, PatStruct, PatTupleStruct, Stmt};

// Diving into AST for variables using nested pattern matching really
// drives me crazy. Although all tests pass, there may be bugs in matches.
#[proc_macro_attribute]
pub fn sorted(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut output: proc_macro2::TokenStream = input.clone().into();
    let item = parse_macro_input!(input as Item);

    match item {
        Item::Enum(ref item_enum) => {
            if let Some(first) = item_enum.variants.first() {
                let mut prev_variant = first;
                for variant in item_enum.variants.iter().skip(1) {
                    if prev_variant.ident > variant.ident {
                        let mut behind = variant;
                        for current_variant in item_enum.variants.iter() {
                            if current_variant.ident > variant.ident {
                                behind = current_variant;
                                break;
                            }
                        }

                        let err = syn::Error::new(
                            variant.span(),
                            format!("{} should sort before {}", variant.ident, behind.ident),
                        );
                        output.extend(err.into_compile_error());
                    }
                    prev_variant = variant;
                }
            }
        },
        _ => {
            let err = syn::Error::new(Span::call_site(), "expected enum or match expression");
            output.extend(err.into_compile_error());
        },
    }

    output.into()
}

#[proc_macro_attribute]
pub fn check(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut item = parse_macro_input!(input as ItemFn);

    for stmt in item.block.stmts.iter_mut() {
        match stmt {
            Stmt::Expr(Expr::Match(expr_match), _) => {
                MatchArmsCheck.visit_expr_match_mut(expr_match);
            },
            _ => {},
        }
    }
    eprintln!("{:#}", item.to_token_stream().to_string());

    quote! { #item }.into()
}

struct MatchArmsCheck;

impl VisitMut for MatchArmsCheck {
    fn visit_expr_match_mut(&mut self, expr_match: &mut ExprMatch) {
        if expr_match.attrs.iter().find(|attr| attr.meta.path().is_ident("sorted")).is_some() {
            expr_match.attrs.retain(|attr| !attr.meta.path().is_ident("sorted"));
            
            match expr_match.arms.first() {
                Some(Arm { pat: Pat::Wild(_), .. }) => {},
                Some(Arm { pat: Pat::Ident(PatIdent { ident, .. }), .. }) => {
                    let mut prev_ident = ident;

                    for arm in expr_match.arms.iter().skip(1) {
                        match arm {
                            Arm { pat: pat @ Pat::Wild(_), .. } => {
                                if let Pat::Wild(_) = expr_match.arms.last().unwrap().pat {} else {
                                    let err = syn::Error::new_spanned(
                                        pat,
                                        "`_` should be at the last arm"
                                    ).into_compile_error();
                                    *expr_match.expr = parse_quote! { #err };
                                }
                            },
                            Arm { pat: Pat::Ident(PatIdent { ident, .. }), .. } => {
                                if prev_ident > ident {
                                    let mut behind = ident;
    
                                    for current_arm in expr_match.arms.iter() {
                                        if let Arm { pat: Pat::Ident(PatIdent { ident: current_ident, .. }), .. } = current_arm {
                                            if current_ident > ident {
                                                behind = current_ident;
                                                break;
                                            }
                                        }
                                    }
    
                                    let err = syn::Error::new(
                                        ident.span(),
                                        format!("{} should sort before {}", ident, behind),
                                    ).into_compile_error();
                                    *expr_match.expr = parse_quote! { #err };
                                }
                                prev_ident = ident;
                            },
                            _ => {},
                        }
                    }
                },
                Some(Arm { pat: Pat::TupleStruct(PatTupleStruct { path, .. }), .. })
                | Some(Arm { pat: Pat::Path(PatPath { path, ..}), .. })
                | Some(Arm { pat: Pat::Struct(PatStruct { path, .. }), .. }) => {
                    let mut prev_variant = path.segments.last().unwrap();

                    for arm in expr_match.arms.iter().skip(1) {
                        match arm {
                            Arm { pat: pat @ Pat::Wild(_), .. } => {
                                if let Pat::Wild(_) = expr_match.arms.last().unwrap().pat {} else {
                                    let err = syn::Error::new_spanned(
                                        pat,
                                        "`_` should be at the last arm"
                                    ).into_compile_error();
                                    *expr_match.expr = parse_quote! { #err };
                                }
                            },
                            Arm { pat: Pat::TupleStruct(PatTupleStruct { path, .. }), .. }
                            | Arm { pat: Pat::Path(PatPath { path, ..}), .. }
                            | Arm { pat: Pat::Struct(PatStruct { path, .. }), .. } => {
                                let variant = path.segments.last().unwrap();
                                if prev_variant.ident > variant.ident {
                                    let mut behind = path;
                                    for current_arm in expr_match.arms.iter() {
                                        match current_arm {
                                            Arm { pat: Pat::TupleStruct(PatTupleStruct { path, .. }), .. }
                                            | Arm { pat: Pat::Path(PatPath { path, ..}), .. }
                                            | Arm { pat: Pat::Struct(PatStruct { path, .. }), .. } => {
                                                let current_variant = path.segments.last().unwrap();
                                                if current_variant.ident > variant.ident {
                                                    behind = path;
                                                    break;
                                                }
                                            },
                                            _ => {}
                                            
                                        }
                                    }

                                    let err = syn::Error::new_spanned(
                                        path,
                                        format!(
                                            "{} should sort before {}",
                                            quote! { #path }.to_string().replace(" ", ""),
                                            quote! { #behind }.to_string().replace(" ", ""),
                                        ),
                                    ).into_compile_error();
                                    *expr_match.expr = parse_quote! { #err };
                                }
                                prev_variant = variant;
                            },
                            _ => {},
                        }
                    }
                },
                Some(Arm { pat, .. }) => {
                    let err = syn::Error::new_spanned(
                        pat,
                        "unsupported by #[sorted]"
                    ).into_compile_error();
                    *expr_match.expr = parse_quote! { #err };
                },
                _ => {},
            }
        }
    }
}

