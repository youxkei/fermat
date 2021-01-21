extern crate cc;

use std::path::PathBuf;

fn main() {
    let dir: PathBuf = ["tree-sitter-erlang", "src"].iter().collect();

    cc::Build::new()
        .warnings(false)
        .include(&dir)
        .file(dir.join("parser.c"))
        .compile("tree-sitter-erlang");
}
