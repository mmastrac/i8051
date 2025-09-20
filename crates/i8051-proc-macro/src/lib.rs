#![doc=include_str!("../README.md")]

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    Expr, ExprAssign, ExprBinary, ExprBlock, ExprCall, ExprField, ExprIf, ExprLet, ExprLit,
    ExprMethodCall, ExprParen, ExprPath, ExprTuple, ExprUnary, Local, Path, PathArguments,
    PathSegment, Token, parse_macro_input, punctuated::Punctuated,
};

/// Desugars and transforms a block of statements into a sequence of
/// op_def_read/of_def_write calls.
#[proc_macro]
pub fn op_def(input: TokenStream) -> TokenStream {
    let mut input = input.into_iter();
    let context = input
        .next()
        .expect("First argument must be the CPU reference");
    let input = TokenStream::from_iter(input);
    let context = TokenStream::from(context);
    let context = parse_macro_input!(context as syn::Ident);
    let input = parse_macro_input!(input as syn::Block);

    let transformed_stmts = input
        .stmts
        .into_iter()
        .map(|stmt| transform_stmt(&context, &stmt))
        .collect::<Vec<_>>();

    let output = quote! {
        #(#transformed_stmts)*
    };

    TokenStream::from(output)
}

fn transform_block(context: &syn::Ident, block: &syn::Block) -> proc_macro2::TokenStream {
    let transformed_stmts = block
        .stmts
        .iter()
        .map(|stmt| transform_stmt(context, stmt))
        .collect::<Vec<_>>();
    quote! {
        {
            #(#transformed_stmts)*
        }
    }
}

fn transform_stmt(context: &syn::Ident, stmt: &syn::Stmt) -> proc_macro2::TokenStream {
    match stmt {
        syn::Stmt::Expr(expr, semi) => {
            let transformed = transform_expr(context, expr);
            quote!(#transformed #semi)
        }
        syn::Stmt::Local(Local {
            let_token,
            attrs,
            pat,
            init,
            semi_token,
        }) => {
            if let Some(init) = init {
                if init.diverge.is_some() {
                    panic!("Unsupported diverge in let statement");
                }
                let transformed_init = transform_expr(context, &init.expr);
                quote! { #(#attrs)* #let_token #pat = #transformed_init #semi_token }
            } else {
                quote! { #(#attrs)* #let_token #pat #semi_token }
            }
        }
        other => quote! { #other },
    }
}

fn transform_expr(context: &syn::Ident, expr: &Expr) -> proc_macro2::TokenStream {
    match expr {
        Expr::Block(ExprBlock { block, .. }) => {
            let transformed_stmts = transform_block(context, block);
            quote! { #transformed_stmts }
        }
        Expr::Let(ExprLet { pat, expr, .. }) => {
            let transformed_init = transform_expr(context, expr);
            quote! { let #pat = #transformed_init; }
        }
        Expr::Tuple(ExprTuple { elems, .. }) => {
            let transformed_elems = elems
                .into_iter()
                .map(|arg| transform_expr(context, arg))
                .collect::<Vec<_>>();
            quote! { (#(#transformed_elems),*) }
        }
        Expr::Assign(ExprAssign { left, right, .. }) => {
            let transformed_right = transform_expr(context, right);
            match &**left {
                // Handle simple variable assignments like A = expr
                Expr::Path(path_expr) => {
                    if let Some(ident) = path_expr.path.get_ident() {
                        match ident.to_string().as_str() {
                            "A" | "B" | "PC" | "DPTR" | "C" | "OV" | "AC" | "Z" => {
                                quote! { op_def_write!(#context, #ident, #transformed_right); }
                            }
                            _ => {
                                let left_expr = left;
                                quote! { #left_expr = #transformed_right; }
                            }
                        }
                    } else {
                        let left_expr = left;
                        quote! { #left_expr = #transformed_right; }
                    }
                }
                // Handle indexed assignments like DATA[index] = expr
                Expr::Index(index_expr) => {
                    let transformed_index = transform_expr(context, &index_expr.index);

                    if let Expr::Path(path_expr) = &*index_expr.expr
                        && let Some(ident) = path_expr.path.get_ident()
                    {
                        match ident.to_string().as_str() {
                            "BIT" | "PBIT" | "CODE" | "XDATA" | "DATA" | "IDATA" | "PDATA"
                            | "R" => {
                                return quote! {{
                                    let index = #transformed_index;
                                    let value = #transformed_right;
                                    op_def_write!(#context, #ident, index, value);
                                }};
                            }
                            _ => {}
                        }
                    }

                    let left_expr = transform_expr(context, left);
                    quote! { #left_expr = #transformed_right; }
                }
                Expr::Tuple(ExprTuple { elems, .. }) => {
                    let tmp = syn::Ident::new("tmp", Span::call_site());
                    let transformed_elems = elems
                        .into_iter()
                        .enumerate()
                        .map(|(i, left)| {
                            transform_expr(
                                context,
                                &Expr::Assign(ExprAssign {
                                    attrs: vec![],
                                    left: Box::new(left.clone()),
                                    eq_token: Token![=](Span::call_site()),
                                    right: Box::new(Expr::Field(ExprField {
                                        attrs: vec![],
                                        base: Box::new(Expr::Path(ExprPath {
                                            attrs: vec![],
                                            qself: None,
                                            path: ident_to_path(&tmp),
                                        })),
                                        dot_token: Token![.](Span::call_site()),
                                        member: syn::Member::Unnamed(syn::Index::from(i)),
                                    })),
                                }),
                            )
                        })
                        .collect::<Vec<_>>();
                    quote! { {
                        let #tmp = #transformed_right;
                        #(#transformed_elems;)*
                    } }
                }
                _ => {
                    panic!("Unsupported left expression");
                }
            }
        }
        Expr::Unary(ExprUnary { op, expr, .. }) => {
            let transformed_expr = transform_expr(context, expr);
            quote! { #op #transformed_expr }
        }
        Expr::Binary(ExprBinary {
            left, op, right, ..
        }) => {
            use syn::BinOp::*;
            match op {
                AddAssign(..) | SubAssign(..) | MulAssign(..) | DivAssign(..) | RemAssign(..)
                | ShlAssign(..) | ShrAssign(..) | BitAndAssign(..) | BitOrAssign(..)
                | BitXorAssign(..) => {
                    let transformed_left = transform_expr(context, left);
                    let transformed_right = transform_expr(context, right);
                    let tmp = syn::Ident::new("tmp", Span::call_site());
                    let assign = transform_expr(
                        context,
                        &Expr::Assign(ExprAssign {
                            attrs: vec![],
                            left: left.clone(),
                            eq_token: Token![=](Span::call_site()),
                            right: Box::new(Expr::Path(ExprPath {
                                attrs: vec![],
                                qself: None,
                                path: ident_to_path(&tmp),
                            })),
                        }),
                    );
                    quote! {{
                        let mut tmp = #transformed_left;
                        tmp #op #transformed_right;
                        #assign;
                    }}
                }
                _ => {
                    let transformed_left = transform_expr(context, left);
                    let transformed_right = transform_expr(context, right);
                    quote! { #transformed_left #op #transformed_right }
                }
            }
        }
        Expr::Call(ExprCall { func, args, .. }) => {
            let transformed_args = args
                .into_iter()
                .map(|arg| transform_expr(context, arg))
                .collect::<Vec<_>>();

            if let Expr::Path(ExprPath { path, .. }) = &**func {
                let ident = path.get_ident().unwrap();
                match ident.to_string().as_str() {
                    "POP" | "POP16" | "PUSH" | "PUSH16" | "CLEAR_INT" | "SEXT" => {
                        return quote! { op_def_call!(#context, #ident (#(#transformed_args),*)) };
                    }
                    _ => {}
                }
            }

            let transformed_func = transform_expr(context, func);
            quote! { #transformed_func(#(#transformed_args),*) }
        }
        Expr::Paren(ExprParen { expr, .. }) => {
            let transformed = transform_expr(context, expr);
            quote! { (#transformed) }
        }
        Expr::MethodCall(ExprMethodCall {
            receiver,
            method,
            args,
            turbofish,
            ..
        }) => {
            if turbofish.is_some() {
                panic!("Turbofish not supported");
            }
            let transformed_expr = transform_expr(context, receiver);
            let transformed_args = args
                .into_iter()
                .map(|arg| transform_expr(context, arg))
                .collect::<Vec<_>>();
            quote! { #transformed_expr.#method(#(#transformed_args),*) }
        }
        Expr::Field(ExprField { base, member, .. }) => {
            let transformed_base = transform_expr(context, base);
            quote! { #transformed_base.#member }
        }
        Expr::Path(path_expr) => {
            if let Some(ident) = path_expr.path.get_ident() {
                match ident.to_string().as_str() {
                    "A" | "B" | "PC" | "DPTR" | "C" | "OV" | "AC" | "Z" => {
                        quote! { op_def_read!(#context, #ident) }
                    }
                    _ => {
                        let expr_ref = &expr;
                        quote! { #expr_ref }
                    }
                }
            } else {
                let expr_ref = &expr;
                quote! { #expr_ref }
            }
        }
        Expr::Index(index_expr) => {
            let transformed_index = transform_expr(context, &index_expr.index);

            match &*index_expr.expr {
                Expr::Path(path_expr) => {
                    if let Some(ident) = path_expr.path.get_ident() {
                        match ident.to_string().as_str() {
                            "BIT" | "PBIT" | "CODE" | "XDATA" | "DATA" | "IDATA" | "PDATA"
                            | "R" => {
                                quote! { op_def_read!(#context, #ident, #transformed_index) }
                            }
                            _ => {
                                let expr_ref = &expr;
                                quote! { #expr_ref }
                            }
                        }
                    } else {
                        let expr_ref = &expr;
                        quote! { #expr_ref }
                    }
                }
                _ => {
                    let expr_ref = &expr;
                    quote! { #expr_ref }
                }
            }
        }
        Expr::If(ExprIf {
            cond,
            then_branch,
            else_branch,
            ..
        }) => {
            let transformed_cond = transform_expr(context, cond);
            let transformed_then = transform_block(context, then_branch);
            if let Some((_, else_branch)) = else_branch {
                let transformed_otherwise = transform_expr(context, else_branch);
                quote! { if #transformed_cond #transformed_then else #transformed_otherwise }
            } else {
                quote! { if #transformed_cond #transformed_then }
            }
        }
        Expr::Lit(ExprLit { lit, .. }) => {
            quote! { #lit }
        }
        _other => panic!("Unsupported expression"),
    }
}

fn ident_to_path(ident: &syn::Ident) -> Path {
    Path {
        leading_colon: None,
        segments: Punctuated::from_iter(vec![PathSegment {
            ident: ident.clone(),
            arguments: PathArguments::None,
        }]),
    }
}
