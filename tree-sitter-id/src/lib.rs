#![feature(proc_macro_quote)]

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

#[proc_macro]
pub fn define_kind_id(_item: TokenStream) -> TokenStream {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_erlang() };
    parser.set_language(language).unwrap();

    let num_node_kinds = language.node_kind_count() as u16;

    let kind_id_variants = (1..num_node_kinds).filter_map(move |kind_id| {
        let kind_name = language.node_kind_for_id(kind_id).unwrap();

        if language.node_kind_is_visible(kind_id) && &kind_name[0..1] != "_" {
            let ident = format_ident!("{}", identize(kind_name).to_ascii_uppercase());

            Some(quote! { #ident = #kind_id })
        } else {
            None
        }
    });

    (quote! {
        #[repr(u16)]
        #[derive(Debug)]
        enum KindId {
            #(#kind_id_variants),*
        }
    })
    .into()
}

fn identize(token: &str) -> &str {
    match token {
        "->" => "DASH_GT",
        "(" => "LPAREN",
        ")" => "RPAREN",
        "[" => "LBRACK",
        "]" => "RBARKC",
        "," => "COMMA",
        "." => "DOT",
        "-" => "DASH",
        "/" => "SLASH",
        ";" => "SEMI",
        ":" => "COLON",
        "\"" => "DQUOTE",
        _ => token,
    }
}
