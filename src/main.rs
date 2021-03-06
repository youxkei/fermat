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
    /// Specifies the line length that fermat will wrap on.
    /// Notice that lengths of comments are treated as zero.
    #[structopt(short, long, default_value = "120")]
    length: i32,

    /// Does not format but remove trailing separators.
    /// This option works with check and write options.
    #[structopt(short, long)]
    no_format: bool,

    /// Checks if the given files are formatted or not.
    /// When one of the given files is not formatted, fermat exits with the status code 1.
    /// With this option, fermat doesn't write output to stdout.
    #[structopt(short, long, conflicts_with("write"))]
    check: bool,

    /// Edits files in-place.
    /// With this option, fermat doesn't write output to stdout.
    #[structopt(short, long, conflicts_with("check"))]
    write: bool,

    /// Files to be formatted
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>,
}

#[paw::main]
fn main(args: Args) -> std::io::Result<()> {
    let config = &Config {
        right_margin: args.length,
        newline_cost: 1,
        beyond_right_margin_cost: 10000,
        height_cost: 100,
        max_choice_nest_level: 10,
    };

    for file in args.files {
        let source_code = fs::read_to_string(&file)?;

        let parse_tree = node::parse(&source_code);

        let formatted_code = if args.no_format {
            trailing_separator_remover::remove_trailing_separators(parse_tree, &source_code)
        } else {
            formatter::format(parse_tree, &source_code, config)
        };

        if args.check {
            if source_code != formatted_code {
                std::process::exit(1);
            }

            continue;
        }

        if args.write {
            fs::write(&file, formatted_code)?;

            continue;
        }

        print!("{}", formatted_code);
    }

    Ok(())
}
