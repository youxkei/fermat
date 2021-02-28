use crate::node::{Node, Tree};

pub fn remove_trailing_separators(tree: Tree, source_code: &str) -> String {
    node_to_removed_code(tree.root_node(), source_code)
}

pub fn node_to_removed_code(node: Node<'_>, source_code: &str) -> String {
    source_code.to_string()
}
