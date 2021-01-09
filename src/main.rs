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

struct KindIds {
    source_file: u16,
    form: u16,
    module_attributes: u16,
    export_attributes: u16,
    export_mfa: u16,
    function: u16,
    function_clause: u16,
    pat_argument_list: u16,
    clause_guard: u16,
    exprs: u16,
    expr: u16,
    function_call: u16,
    expr_remote: u16,
    argument_list: u16,
    atomic: u16,
    atom: u16,
    integer: u16,
    strings: u16,
    string: u16,
    comment: u16,
    line_comment: u16,
    multiple_newlines: u16,
}

struct FieldIds {
    arrow: u16,
    semicolon: u16,
}

struct Ids {
    kind: KindIds,
    field: FieldIds,
}

use tree_sitter_id::kind_id_enum;

kind_id_enum!();

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

    println!(
        "{:?}",
        language.node_kind_is_visible(KindId::_ESCAPE_SEQUENCE as u16)
    );

    let ids = Ids {
        kind: KindIds {
            source_file: language.id_for_node_kind("source_file", true),
            form: language.id_for_node_kind("form", true),
            module_attributes: language.id_for_node_kind("module_attributes", true),
            export_attributes: language.id_for_node_kind("export_attributes", true),
            export_mfa: language.id_for_node_kind("export_mfa", true),
            function: language.id_for_node_kind("function", true),
            function_clause: language.id_for_node_kind("function_clause", true),
            pat_argument_list: language.id_for_node_kind("pat_argument_list", true),
            clause_guard: language.id_for_node_kind("clause_guard", true),
            exprs: language.id_for_node_kind("exprs", true),
            expr: language.id_for_node_kind("expr", true),
            function_call: language.id_for_node_kind("function_call", true),
            expr_remote: language.id_for_node_kind("expr_remote", true),
            argument_list: language.id_for_node_kind("argument_list", true),
            atomic: language.id_for_node_kind("atomic", true),
            atom: language.id_for_node_kind("atom", true),
            integer: language.id_for_node_kind("integer", true),
            strings: language.id_for_node_kind("strings", true),
            string: language.id_for_node_kind("string", true),
            comment: language.id_for_node_kind("comment", true),
            line_comment: language.id_for_node_kind("line_comment", true),
            multiple_newlines: language.id_for_node_kind("multiple_newlines", true),
        },
        field: FieldIds {
            arrow: language.field_id_for_name("arrow").unwrap(),
            semicolon: language.field_id_for_name("semicolon").unwrap(),
        },
    };

    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();

    node_to_layout_expr(root_node, &ids, source_code)
}

fn node_to_layout_expr<'a>(node: Node<'_>, ids: &Ids, source_code: &'a str) -> Rc<LayoutExpr<'a>> {
    if !node.is_named() {
        text!(&source_code[node.start_byte()..node.end_byte()])
    } else {
        let kind_id: KindId = unsafe { std::mem::transmute(node.kind_id()) };

        match kind_id {
            KindId::ATOM
            | KindId::INTEGER
            | KindId::STRING
            | KindId::COMMENT
            | KindId::LINE_COMMENT => {
                text!(&source_code[node.start_byte()..node.end_byte()])
            }

            KindId::MULTIPLE_NEWLINES => text!(""),

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
                assert!(
                    cursor.goto_first_child(),
                    "{:?} should have its children",
                    node
                );

                loop {
                    let child = cursor.node();

                    match phase {
                        Phase::BeforeArrow => {
                            if child.kind_id() == ids.kind.comment {
                                before_body = apposition!(
                                    before_body,
                                    text!(" "),
                                    stack!(node_to_layout_expr(child, ids, source_code), text!(""))
                                )
                            } else if child.kind_id() == ids.kind.line_comment {
                                before_body = apposition!(
                                    before_body,
                                    stack!(
                                        text!(""),
                                        node_to_layout_expr(child, ids, source_code),
                                        text!("")
                                    )
                                )
                            } else if cursor.field_id() == Some(ids.field.arrow) {
                                before_body = apposition!(
                                    before_body,
                                    text!(" "),
                                    node_to_layout_expr(child, ids, source_code)
                                );

                                phase = Phase::AfterArrow
                            } else {
                                before_body = apposition!(
                                    before_body,
                                    node_to_layout_expr(child, ids, source_code),
                                )
                            }
                        }

                        Phase::AfterArrow => {
                            if child.kind_id() == ids.kind.comment {
                                before_body = apposition!(
                                    before_body,
                                    text!(" "),
                                    node_to_layout_expr(child, ids, source_code),
                                )
                            } else {
                                body = stack!(body, node_to_layout_expr(child, ids, source_code));
                            }

                            phase = Phase::InBody
                        }

                        Phase::InBody => {
                            if child.kind_id() == ids.kind.comment {
                                body =
                                    apposition!(body, node_to_layout_expr(child, ids, source_code))
                            } else {
                                body = stack!(body, node_to_layout_expr(child, ids, source_code))
                            }
                        }
                    }

                    if !cursor.goto_next_sibling() {
                        break;
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

            KindId::FUNCTION => {
                let mut result = unit!();
                let mut line = unit!();

                let mut cursor = node.walk();
                assert!(
                    cursor.goto_first_child(),
                    "{:?} should have its children",
                    node
                );

                loop {
                    let child = cursor.node();

                    if cursor.field_id() == Some(ids.field.semicolon) {
                        result = stack!(
                            result,
                            apposition!(line, node_to_layout_expr(child, ids, source_code))
                        );
                        line = unit!();
                    } else {
                        let child_kind_id = child.kind_id();

                        if child_kind_id == ids.kind.comment
                            || child_kind_id == ids.kind.function_clause
                        {
                            result = stack!(result, line);
                            line = node_to_layout_expr(child, ids, source_code);
                        } else {
                            panic!("node {} cannot be occured here", child.kind())
                        }
                    }

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }

                result = stack!(result, line);

                result
            }

            KindId::SOURCE_FILE => {
                let mut result = unit!();

                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    result = stack!(result, node_to_layout_expr(child, ids, source_code))
                }

                result
            }
            _ => {
                let mut result = unit!();

                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    result = apposition!(result, node_to_layout_expr(child, ids, source_code));
                }

                result
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_parse {
        ($text:expr) => {
            let text = $text;
            assert_eq!(parse(text).format(0).0 + "\n", text)
        };
    }

    #[test]
    fn function_clause_test() {
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
    fn function_test() {
        assert_parse!(indoc! {r#"
            f() ->
                foo;
            f() ->
                bar.
        "#});
    }
}
