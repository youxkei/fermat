#![feature(box_syntax, box_patterns, bindings_after_at, decl_macro, or_patterns)]

mod avltree;
mod formatter;
mod layout_expr;
mod layout_fun;
mod node;
mod trailing_separator_remover;

use std::fs;
use std::path::PathBuf;

use structopt::StructOpt;

use layout_fun::Config;

/// A source code formatter for Erlang.
/// By default, output is written to stdout.
#[derive(StructOpt)]
struct Args {
    /// Do not format but remove trailing separators.
    /// This option works with check and write options.
    #[structopt(short, long, conflicts_with("write"))]
    no_format: bool,

    /// Check if the given file is formatted or not.
    /// When the file is not formatted, fermat exits with the status code 1.
    /// With this option, fermat doesn't write output to stdout.
    #[structopt(short, long, conflicts_with("write"))]
    check: bool,

    /// Edit file in-place.
    /// With this option, fermat doesn't write output to stdout.
    #[structopt(short, long, conflicts_with("check"))]
    write: bool,

    /// A file to be formatted
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

#[paw::main]
fn main(args: Args) -> std::io::Result<()> {
    let source_code = fs::read_to_string(&args.file)?;

    let config = &Config {
        right_margin: 120,
        newline_cost: 1,
        beyond_right_margin_cost: 10000,
        height_cost: 100,
        max_choice_nest_level: 10,
    };

    let parse_tree = node::parse(&source_code);

    let formatted_code = if args.no_format {
        trailing_separator_remover::remove_trailing_separators(parse_tree, &source_code)
    } else {
        formatter::format(parse_tree, &source_code, config)
    };

    if args.check {
        if source_code == formatted_code {
            std::process::exit(0);
        } else {
            std::process::exit(1);
        }
    }

    if args.write {
        fs::write(&args.file, formatted_code)?;
        return Ok(());
    }

    print!("{}", formatted_code);

    Ok(())
}
