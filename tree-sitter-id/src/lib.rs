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

    let mut kind_ids = vec![];

    for kind_id in 1..num_node_kinds {
        let kind_name = language.node_kind_for_id(kind_id).unwrap();

        if language.node_kind_is_visible(kind_id) && &kind_name[0..1] != "_" {
            kind_ids.push((kind_id, identize(kind_name).to_ascii_uppercase()));
        }
    }

    let kind_id_variants = kind_ids.iter().map(move |(kind_id, kind_name)| {
        let ident = format_ident!("{}", kind_name);

        Some(quote! { #ident = #kind_id, })
    });

    let open_kind_id_idents = kind_ids.iter().filter_map(move |(_, kind_name)| {
        if kind_name.ends_with("_OPEN") {
            let ident = format_ident!("{}", kind_name);
            Some(quote! { #ident })
        } else {
            None
        }
    });

    let close_kind_id_idents = kind_ids.iter().filter_map(move |(_, kind_name)| {
        if kind_name.ends_with("_CLOSE") {
            let ident = format_ident!("{}", kind_name);
            Some(quote! { #ident })
        } else {
            None
        }
    });

    let op_kind_id_idents = kind_ids.iter().filter_map(move |(_, kind_name)| {
        if kind_name.ends_with("_OP") {
            let ident = format_ident!("{}", kind_name);
            Some(quote! { #ident })
        } else {
            None
        }
    });

    (quote! {
        #[repr(u16)]
        #[derive(Debug, Copy, Clone, PartialEq)]
        enum KindId {
            #(#kind_id_variants)*
            ERROR = 65535,
        }

        impl KindId {
            fn is_open(&self) -> bool {
                match self {
                    #(KindId::#open_kind_id_idents)|* => true,
                    _ => false,
                }
            }

            fn is_close(&self) -> bool {
                match self {
                    #(KindId::#close_kind_id_idents)|* => true,
                    _ => false,
                }
            }

            fn is_op(&self) -> bool {
                match self {
                    #(KindId::#op_kind_id_idents)|* => true,
                    _ => false,
                }
            }
        }
    })
    .into()
}

fn identize(token: &str) -> &str {
    match token {
        "->" => "HYPHEN_GT",
        "(" => "PAREN_OPEN",
        ")" => "PAREN_CLOSE",
        "[" => "BRACKET_OPEN",
        "]" => "BRACKET_CLOSE",
        "," => "COMMA",
        "." => "PERIOD",
        "-" => "HYPHEN",
        "/" => "SLASH",
        ";" => "SEMICOLON",
        ":" => "COLON",
        "\"" => "DOUBLE_QUOTE",
        "=" => "EQUAL",
        "+" => "PLUS",
        "!" => "EXCLAM",
        "*" => "ASTERISK",
        "++" => "PLUSPLUS",
        "--" => "HYPHENHYPHEN",
        _ => token,
    }
}
