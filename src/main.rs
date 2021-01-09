#![feature(box_syntax, box_patterns, bindings_after_at)]

mod avltree;
#[macro_use]
mod layout_expr;
mod layout_fun;

use std::fs;
use std::iter::repeat;
use std::rc::Rc;

use tree_sitter::{Language, Node, Parser};

use layout_expr::LayoutExpr;
use layout_fun::{Config, LayoutFun};

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

tree_sitter_id::define_kind_id!();

fn main() {
    let source_code = fs::read_to_string("hello.erl").unwrap();
    let layout_expr = parse(&source_code);

    let config = Config {
        right_margin: 36,
        newline_cost: 1,
        beyond_right_margin_cost: 100,
    };

    println!(
        "{}|",
        repeat(' ')
            .take((config.right_margin - 1) as usize)
            .collect::<String>()
    );

    let layout_fun = LayoutFun::from_layout_expr(&*layout_expr, &config);
    let layout_expr = layout_fun.at(0).layout_expr;

    layout_expr.print(0);
}

fn parse(source_code: &str) -> Rc<LayoutExpr<'_>> {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_erlang() };
    parser.set_language(language).unwrap();

    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();

    node_to_layout_expr(root_node, source_code)
}

fn node_to_layout_expr<'a>(node: Node<'_>, source_code: &'a str) -> Rc<LayoutExpr<'a>> {
    match kind_id(node) {
        KindId::SOURCE_FILE => {
            let mut result = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                result = stack!(result, node_to_layout_expr(child, source_code))
            }

            result
        }

        KindId::FUNCTION => {
            let mut result = unit!();
            let mut line = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMENT => {
                        line =
                            apposition!(line, text!(" "), node_to_layout_expr(child, source_code));
                    }

                    KindId::SEMI => {
                        line = apposition!(line, node_to_layout_expr(child, source_code));
                    }

                    KindId::FUNCTION_CLAUSE | KindId::LINE_COMMENT | KindId::MULTIPLE_NEWLINES => {
                        result = stack!(result, line);
                        line = node_to_layout_expr(child, source_code);
                    }

                    _ => {
                        panic!("{:?} should not have {:?}", node, child)
                    }
                }
            }

            stack!(result, line)
        }

        KindId::FUNCTION_CLAUSE => {
            let mut before_body = unit!();
            let mut body = unit!();

            enum Phase {
                BeforeArrow,
                AfterArrow,
                InBody,
            }
            let mut phase = Phase::BeforeArrow;

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                match phase {
                    Phase::BeforeArrow => match kind_id(child) {
                        KindId::COMMENT => {
                            before_body = apposition!(
                                before_body,
                                text!(" "),
                                stack!(node_to_layout_expr(child, source_code), text!(""))
                            )
                        }

                        KindId::LINE_COMMENT => {
                            before_body = apposition!(
                                before_body,
                                stack!(
                                    text!(""),
                                    node_to_layout_expr(child, source_code),
                                    text!("")
                                )
                            )
                        }

                        KindId::DASH_GT => {
                            before_body = apposition!(
                                before_body,
                                text!(" "),
                                node_to_layout_expr(child, source_code)
                            );

                            phase = Phase::AfterArrow
                        }

                        _ => {
                            before_body =
                                apposition!(before_body, node_to_layout_expr(child, source_code),)
                        }
                    },

                    Phase::AfterArrow => {
                        match kind_id(child) {
                            KindId::COMMENT => {
                                before_body = apposition!(
                                    before_body,
                                    text!(" "),
                                    node_to_layout_expr(child, source_code),
                                )
                            }

                            _ => {
                                body = stack!(body, node_to_layout_expr(child, source_code));
                            }
                        }

                        phase = Phase::InBody
                    }

                    Phase::InBody => match kind_id(child) {
                        KindId::COMMENT => {
                            body = apposition!(body, node_to_layout_expr(child, source_code))
                        }

                        _ => body = stack!(body, node_to_layout_expr(child, source_code)),
                    },
                }
            }

            choice!(
                stack!(
                    before_body.clone(),
                    apposition!(text!("    "), body.clone())
                ),
                apposition!(before_body, text!(" "), body),
            )
        }

        KindId::EXPRS => {
            let mut result = unit!();
            let mut line = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMA => {
                        line = apposition!(line, node_to_layout_expr(child, source_code));
                    }

                    KindId::COMMENT => {
                        line =
                            apposition!(line, text!(" "), node_to_layout_expr(child, source_code));
                    }

                    KindId::LINE_COMMENT | KindId::EXPR => {
                        result = stack!(result, line);
                        line = node_to_layout_expr(child, source_code);
                    }

                    _ => {
                        panic!("{:?} should not have {:?}", node, child)
                    }
                }
            }

            stack!(result, line)
        }

        KindId::STRINGS => {
            let mut result = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                result = stack!(result, node_to_layout_expr(child, source_code));
            }

            result
        }

        KindId::FORM
        | KindId::MODULE_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE
        | KindId::EXPORT_MFA
        | KindId::PAT_ARGUMENT_LIST
        | KindId::CLAUSE_GUARD
        | KindId::EXPR
        | KindId::FUNCTION_CALL
        | KindId::EXPR_REMOTE
        | KindId::EXPR_MAX
        | KindId::ARGUMENT_LIST => {
            let mut result = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                result = apposition!(result, node_to_layout_expr(child, source_code));
            }

            result
        }

        KindId::DASH_GT
        | KindId::LPAREN
        | KindId::RPAREN
        | KindId::LBRACK
        | KindId::RBARKC
        | KindId::COMMA
        | KindId::DOT
        | KindId::DASH
        | KindId::SLASH
        | KindId::SEMI
        | KindId::COLON
        | KindId::DQUOTE
        | KindId::MODULE
        | KindId::EXPORT
        | KindId::WHEN
        | KindId::ATOM
        | KindId::INTEGER
        | KindId::STRING
        | KindId::COMMENT
        | KindId::LINE_COMMENT => {
            text!(&source_code[node.start_byte()..node.end_byte()])
        }

        KindId::MULTIPLE_NEWLINES => text!(""),
    }
}

#[cfg(test)]
mod node_to_layout_expr_test {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_parse {
        ($text:expr) => {
            let text = $text;
            assert_eq!(parse(text).format(0).0 + "\n", text)
        };
    }

    #[test]
    fn function_clause() {
        assert_parse!(indoc! {r#"
            main() ->
                io:format("Hello, world!").
        "#});

        assert_parse!(indoc! {r#"
            main % after main
                 () % after ()
                     -> % after ->
                %% line comment 1
                %% line comment 2
                io:format("Hello, world!").
        "#});
    }

    #[test]
    fn function() {
        assert_parse!(indoc! {r#"
            f() -> % comment 1
                foo;

            %% line comment

            f() -> % comment 2
                bar.
        "#});
    }
}

fn kind_id(node: Node<'_>) -> KindId {
    unsafe { std::mem::transmute(node.kind_id()) }
}
