extern crate erl_tokenize;
extern crate fermat;
extern crate glob;
extern crate serde_derive;
extern crate structopt;

use erl_tokenize::Tokenizer;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    /// Target rust source files
    globs: Vec<String>,
}

fn main() {
    let opt = Opt::from_args();

    match opt {
        Opt { globs, .. } => {
            let mut paths = HashSet::new();

            for glob in &globs {
                for path in glob::glob(glob).unwrap().filter_map(Result::ok) {
                    paths.insert(path);
                }
            }

            for path in paths {
                let mut src = String::new();
                let mut file = File::open(path).expect("Cannot open file");
                file.read_to_string(&mut src).expect("Cannot read file");

                let tokens = Tokenizer::new(&src)
                    .filter(|token| match token {
                        Ok(token) => match token.as_whitespace_token() {
                            Some(_) => false,
                            _ => true,
                        },
                        _ => true,
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                println!("{:?}", *fermat::layout_expr::parse(tokens.as_slice()));
            }
        }
    }
}
