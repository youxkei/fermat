use tree_sitter::{Language, Parser};
pub use tree_sitter::{Node, Tree};

tree_sitter_id::define_kind_id! {}

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

pub fn parse(source_code: &str) -> Tree {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_erlang() };
    parser.set_language(language).unwrap();

    parser.parse(source_code, None).unwrap()
}

pub fn kind_id(node: Node<'_>) -> KindId {
    unsafe { std::mem::transmute(node.kind_id()) }
}
