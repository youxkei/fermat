#![feature(proc_macro_quote)]

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

#[proc_macro]
pub fn kind_id_enum(_item: TokenStream) -> TokenStream {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_erlang() };
    parser.set_language(language).unwrap();

    let num_node_kinds = language.node_kind_count() as u16;

    let variants = (1..num_node_kinds).filter_map(move |node_kind| {
        if language.node_kind_is_named(node_kind) && language.node_kind_is_visible(node_kind) {
            let ident = format_ident!(
                "{}",
                language
                    .node_kind_for_id(node_kind)
                    .unwrap()
                    .to_ascii_uppercase()
            );

            Some(quote! {#ident = #node_kind})
        } else {
            None
        }
    });

    (quote! {
        #[repr(u16)]
        #[derive(Debug)]
        enum KindId {
            #(#variants),*
        }
    })
    .into()
}
