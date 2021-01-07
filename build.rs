extern crate cc;

use std::path::PathBuf;

fn main() {
    let dir: PathBuf = ["tree-sitter-erlang", "src"].iter().collect();

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .compile("tree-sitter-erlang");
}
