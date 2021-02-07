#![feature(box_syntax, box_patterns, bindings_after_at, decl_macro, or_patterns)]

mod avltree;
mod layout_expr;
mod layout_fun;

use std::fs;
use std::rc::Rc;

use tree_sitter::{Language, Node, Parser};

use layout_expr::{apposition, apposition_sep, choice, stack, text, unit, LayoutExpr};
use layout_fun::{Config as LayoutFunConfig, LayoutFun};

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

tree_sitter_id::define_kind_id! {}

struct Config {
    max_choice_nest_level: u8,
}

fn main() {
    let source_code = fs::read_to_string("hello.erl").unwrap();

    let layout_fun_config = &LayoutFunConfig {
        right_margin: 50,
        newline_cost: 1,
        beyond_right_margin_cost: 10000,
        height_cost: 100,
    };

    let config = &Config {
        max_choice_nest_level: 6,
    };

    println!("{}", format(&source_code, config, layout_fun_config));
}

fn format(source_code: &str, config: &Config, layout_fun_config: &LayoutFunConfig) -> String {
    let layout_expr = parse(source_code, config);
    let layout_fun = LayoutFun::from_layout_expr(&layout_expr, layout_fun_config);

    let calculated = layout_fun.at(0);

    calculated.layout_expr.format(0, false).0
}

fn parse<'a>(source_code: &'a str, config: &Config) -> Rc<LayoutExpr<'a>> {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_erlang() };
    parser.set_language(language).unwrap();

    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();

    node_to_layout_expr(root_node, source_code, config, 0)
}

fn node_to_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let kind_id = kind_id(node);

    match kind_id {
        KindId::FORM
        | KindId::MODULE_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE_MFA
        | KindId::FUNCTION_CLAUSE_OPEN
        | KindId::CLAUSE_GUARD
        | KindId::FUNCTION_CALL_OPEN
        | KindId::UNARY_EXPR
        | KindId::REMOTE_EXPR
        | KindId::PAT_UNARY_EXPR
        | KindId::ORELSE_OP
        | KindId::ANDALSO_OP
        | KindId::EQUAL_OP
        | KindId::EXCLAM_OP
        | KindId::COMP_OP
        | KindId::LIST_OP
        | KindId::ADD_OP
        | KindId::MULT_OP
        | KindId::PREFIX_OP
        | KindId::LIST_CLOSE
        | KindId::LIST_TAIL
        | KindId::BINARY_ELEMENT
        | KindId::PAT_LIST_CLOSE
        | KindId::PAT_LIST_TAIL
        | KindId::PAT_BINARY_ELEMENT => {
            elements_node_to_apposed_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::SOURCE_FILE
        | KindId::EXPORT_ATTRIBUTE_MFAS
        | KindId::FUNCTION_CLAUSES
        | KindId::FUNCTION_CLAUSE
        | KindId::PAT_ARGUMENT_LIST
        | KindId::FUNCTION_CALL
        | KindId::GUARD
        | KindId::EXPRS
        | KindId::LIST
        | KindId::BINARY
        | KindId::PAT_LIST
        | KindId::PAT_BINARY
        | KindId::STRINGS => {
            elements_node_to_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::BINARY_EXPR | KindId::PAT_BINARY_EXPR => {
            binary_expression_node_to_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::HYPHEN_GREATER
        | KindId::PAREN_OPEN
        | KindId::PAREN_CLOSE
        | KindId::BRACKET_OPEN
        | KindId::BRACKET_CLOSE
        | KindId::LESS_LESS_OPEN
        | KindId::GREATER_GREATER_CLOSE
        | KindId::COMMA
        | KindId::PERIOD
        | KindId::HYPHEN
        | KindId::SLASH
        | KindId::SEMICOLON
        | KindId::COLON
        | KindId::QUOTEDBL
        | KindId::EQUAL
        | KindId::PLUS
        | KindId::EXCLAM
        | KindId::ASTERISK
        | KindId::MODULE
        | KindId::EXPORT
        | KindId::WHEN
        | KindId::CATCH
        | KindId::BOR
        | KindId::BXOR
        | KindId::BSL
        | KindId::BSR
        | KindId::OR
        | KindId::XOR
        | KindId::DIV
        | KindId::REM
        | KindId::BAND
        | KindId::AND
        | KindId::ORELSE
        | KindId::ANDALSO
        | KindId::BNOT
        | KindId::NOT
        | KindId::PLUS_PLUS
        | KindId::HYPHEN_HYPHEN
        | KindId::EQUAL_EQUAL
        | KindId::SLASH_EQUAL
        | KindId::EQUAL_LESS
        | KindId::LESS
        | KindId::GREATER_EQUAL
        | KindId::GREATER
        | KindId::EQUAL_COLON_EQUAL
        | KindId::EQUAL_SLASH_EQUAL
        | KindId::BAR
        | KindId::VARIABLE
        | KindId::ATOM
        | KindId::INTEGER
        | KindId::STRING
        | KindId::COMMENT
        | KindId::LINE_COMMENT => text!(&source_code[node.start_byte()..node.end_byte()]),

        KindId::MULTIPLE_NEWLINES => text!(""),

        KindId::ERROR => panic!("syntax error {:?}", node),
    }
}

fn elements_node_to_apposed_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let mut result = unit!();
    let mut comments = unit!();

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::MULTIPLE_NEWLINES => {}

            KindId::COMMENT | KindId::LINE_COMMENT => {
                comments = stack!(
                    comments,
                    node_to_layout_expr(child, source_code, config, choice_nest_level)
                )
            }

            KindId::HYPHEN_GREATER
            | KindId::WHEN
            | KindId::GUARD
            | KindId::BAR
            | KindId::LIST_TAIL
            | KindId::PAT_LIST_TAIL => {
                result = apposition!(
                    result,
                    text!(" "),
                    stack!(
                        comments,
                        node_to_layout_expr(child, source_code, config, choice_nest_level)
                    )
                );

                comments = unit!();
            }

            KindId::CATCH => {
                result = apposition!(
                    node_to_layout_expr(child, source_code, config, choice_nest_level),
                    text!(" "),
                )
            }

            _ => {
                result = apposition!(
                    result,
                    if comments == unit!() {
                        unit!()
                    } else {
                        text!(" ")
                    },
                    stack!(
                        comments,
                        node_to_layout_expr(child, source_code, config, choice_nest_level)
                    )
                );

                comments = unit!();
            }
        }
    }

    result
}

fn elements_node_to_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let kind_id = kind_id(node);

    let choice_nest = match kind_id {
        KindId::SOURCE_FILE | KindId::FUNCTION_CLAUSES | KindId::STRINGS => 0,
        KindId::EXPORT_ATTRIBUTE_MFAS => 1,
        KindId::FUNCTION_CLAUSE => 1,
        KindId::FUNCTION_CALL => 2,
        KindId::PAT_ARGUMENT_LIST
        | KindId::GUARD
        | KindId::EXPRS
        | KindId::LIST
        | KindId::BINARY
        | KindId::PAT_LIST
        | KindId::PAT_BINARY => 1,
        _ => panic!("{:?} is not covered", kind_id),
    };

    let mut elements = vec![];

    let mut open = unit!();
    let mut close = unit!();
    let mut element = unit!();
    let mut separator = unit!();
    let mut comments = unit!();
    let mut extras = unit!();
    let mut last_extra = unit!();

    let mut after_open = false;
    let mut has_extra = false;
    let mut last_comment = false;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::COMMENT => {
                after_open = false;

                if extras.is_unit() && last_extra.is_unit() {
                    comments = stack!(
                        comments,
                        node_to_layout_expr(
                            child,
                            source_code,
                            config,
                            choice_nest_level + choice_nest
                        )
                    )
                } else {
                    extras = stack!(extras, last_extra);
                    last_extra = node_to_layout_expr(
                        child,
                        source_code,
                        config,
                        choice_nest_level + choice_nest,
                    )
                }

                has_extra = true;
                last_comment = true;
            }

            kind_id @ (KindId::MULTIPLE_NEWLINES | KindId::LINE_COMMENT) => {
                if after_open {
                    after_open = false;

                    if kind_id == KindId::MULTIPLE_NEWLINES {
                        continue;
                    }
                }

                if last_extra.is_empty() {
                    has_extra = true;
                }
                extras = stack!(extras, last_extra);
                last_extra = node_to_layout_expr(
                    child,
                    source_code,
                    config,
                    choice_nest_level + choice_nest,
                );
                if !last_extra.is_empty() {
                    has_extra = true;
                }

                match kind_id {
                    KindId::MULTIPLE_NEWLINES => last_comment = !extras.is_unit(),
                    KindId::LINE_COMMENT => last_comment = true,
                    _ => panic!("{:?} is not covered", kind_id),
                }
            }

            KindId::COMMA | KindId::SEMICOLON => {
                after_open = false;

                if !comments.is_unit() || !extras.is_unit() || !last_extra.is_unit() {
                    if last_extra.is_empty() {
                        has_extra = true;
                    }

                    elements.push(apposition_sep!(text!(" "), element, comments));
                    elements.push(stack!(extras, last_extra));

                    element = unit!();
                    comments = unit!();
                    extras = unit!();
                    last_extra = unit!();
                }

                separator =
                    node_to_layout_expr(child, source_code, config, choice_nest_level + choice_nest)
            }

            kind_id => {
                after_open = false;

                if kind_id.is_open() {
                    open = node_to_layout_expr(
                        child,
                        source_code,
                        config,
                        choice_nest_level + choice_nest,
                    );
                    after_open = true;

                    continue;
                }

                if kind_id.is_close() {
                    close = node_to_layout_expr(
                        child,
                        source_code,
                        config,
                        choice_nest_level + choice_nest,
                    );

                    continue;
                }

                if last_extra.is_empty() {
                    has_extra = true;
                }

                element = apposition!(element, separator);

                elements.push(apposition_sep!(text!(" "), element, comments));
                elements.push(stack!(extras, last_extra));

                element = node_to_layout_expr(
                    child,
                    source_code,
                    config,
                    choice_nest_level + choice_nest,
                );

                separator = unit!();
                comments = unit!();
                extras = unit!();
                last_extra = unit!();

                last_comment = false;
            }
        }
    }

    if !last_extra.is_empty() {
        extras = stack!(extras, last_extra)
    }

    if !comments.is_unit() || !extras.is_unit() {
        last_comment = true;
    }

    elements.push(apposition_sep!(text!(" "), element, comments));
    elements.push(extras);

    let mut num_elements = 0;

    let mut stacked_elements = unit!();
    let mut apposed_elements = unit!();

    for element in elements {
        if element.is_unit() {
            continue;
        }

        num_elements += 1;

        stacked_elements = stack!(stacked_elements, element.clone());

        if apposed_elements == unit!() {
            apposed_elements = element;
        } else {
            apposed_elements = apposition!(apposed_elements, text!(" "), element);
        }
    }

    match kind_id {
        KindId::SOURCE_FILE => stacked_elements,

        KindId::FUNCTION_CLAUSES | KindId::STRINGS => {
            if last_comment {
                stack!(stacked_elements, text!(""))
            } else {
                stacked_elements
            }
        }

        KindId::EXPORT_ATTRIBUTE_MFAS => {
            if has_extra || last_comment {
                stack!(open, apposition!(text!(" "), stacked_elements), close)
            } else {
                choice!(
                    stack!(
                        open.clone(),
                        apposition!(text!(" "), stacked_elements),
                        close.clone()
                    ),
                    apposition!(open, apposed_elements, close)
                )
            }
        }

        KindId::FUNCTION_CLAUSE => {
            if num_elements > 1 {
                stack!(
                    open,
                    apposition!(
                        text!("    "),
                        stack!(
                            stacked_elements,
                            if last_comment { text!("") } else { unit!() }
                        )
                    )
                )
            } else {
                apposition!(
                    choice!(
                        stack!(open.clone(), text!("    ")),
                        apposition!(open, text!(" "))
                    ),
                    stack!(
                        stacked_elements,
                        if last_comment { text!("") } else { unit!() }
                    )
                )
            }
        }

        KindId::FUNCTION_CALL => {
            let body = if has_extra || choice_nest_level > config.max_choice_nest_level {
                stacked_elements
            } else {
                choice!(stacked_elements, apposed_elements)
            };

            let tail = if last_comment {
                stack!(body, close)
            } else {
                apposition!(body, close)
            };

            if choice_nest_level > config.max_choice_nest_level {
                stack!(open, apposition!(text!("    "), tail))
            } else {
                apposition!(choice!(stack!(open.clone(), text!("  ")), open), tail)
            }
        }

        KindId::PAT_ARGUMENT_LIST
        | KindId::GUARD
        | KindId::EXPRS
        | KindId::LIST
        | KindId::BINARY
        | KindId::PAT_LIST
        | KindId::PAT_BINARY => {
            let body = if choice_nest_level > config.max_choice_nest_level {
                stacked_elements
            } else {
                choice!(stacked_elements, apposed_elements)
            };

            apposition!(open, body, close)
        }

        _ => panic!("{:?} is not covered", kind_id),
    }
}

fn binary_expression_node_to_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let mut lhs = unit!();
    let mut comments_between_lhs_and_op = unit!();
    let mut op = unit!();
    let mut op_kind_id = KindId::ERROR;
    let mut comments_between_op_and_rhs = unit!();
    let mut rhs = unit!();

    enum Phase {
        Lhs,
        Rhs,
    }
    let mut phase = Phase::Lhs;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match phase {
            Phase::Lhs => match self::kind_id(child) {
                KindId::MULTIPLE_NEWLINES => {}

                KindId::COMMENT | KindId::LINE_COMMENT => {
                    comments_between_lhs_and_op = stack!(
                        comments_between_lhs_and_op,
                        node_to_layout_expr(child, source_code, config, choice_nest_level + 1)
                    )
                }

                kind_id => {
                    if kind_id.is_op() {
                        op = node_to_layout_expr(child, source_code, config, choice_nest_level + 1);
                        op_kind_id = kind_id;
                        phase = Phase::Rhs;
                    } else {
                        lhs =
                            node_to_layout_expr(child, source_code, config, choice_nest_level + 1);
                    }
                }
            },

            Phase::Rhs => match self::kind_id(child) {
                KindId::MULTIPLE_NEWLINES => {}

                KindId::COMMENT | KindId::LINE_COMMENT => {
                    comments_between_op_and_rhs = stack!(
                        comments_between_op_and_rhs,
                        node_to_layout_expr(child, source_code, config, choice_nest_level)
                    )
                }

                _ => rhs = node_to_layout_expr(child, source_code, config, choice_nest_level),
            },
        }
    }

    match op_kind_id {
        KindId::EXCLAM_OP
        | KindId::ORELSE_OP
        | KindId::ANDALSO_OP
        | KindId::COMP_OP
        | KindId::LIST_OP
        | KindId::ADD_OP
        | KindId::MULT_OP => {
            apposition!(
                choice!(
                    stack!(
                        apposition!(
                            lhs.clone(),
                            if comments_between_lhs_and_op.is_unit() {
                                unit!()
                            } else {
                                apposition!(text!(" "), comments_between_lhs_and_op.clone())
                            },
                        ),
                        text!("")
                    ),
                    apposition!(
                        lhs,
                        text!(" "),
                        if comments_between_lhs_and_op.is_unit() {
                            unit!()
                        } else {
                            stack!(comments_between_lhs_and_op, text!(""))
                        },
                    )
                ),
                apposition!(op, text!(" "), stack!(comments_between_op_and_rhs, rhs))
            )
        }

        KindId::EQUAL_OP => {
            apposition!(
                choice!(
                    stack!(
                        apposition!(
                            lhs.clone(),
                            text!(" "),
                            stack!(comments_between_lhs_and_op.clone(), op.clone()),
                        ),
                        text!("    ")
                    ),
                    apposition!(
                        lhs,
                        text!(" "),
                        if comments_between_lhs_and_op.is_unit() {
                            op
                        } else {
                            stack!(comments_between_lhs_and_op, op)
                        },
                        text!(" "),
                    )
                ),
                stack!(comments_between_op_and_rhs, rhs)
            )
        }

        _ => panic!("{:?} is not covered", op_kind_id),
    }
}

fn kind_id(node: Node<'_>) -> KindId {
    unsafe { std::mem::transmute(node.kind_id()) }
}

#[cfg(test)]
mod parse_test {
    use crate::{parse, Config};

    macro assert_parse {
        ($source_code:expr $(,)?) => {{
            let source_code = $source_code;
            let config = &Config {
                max_choice_nest_level: 100,
            };
            pretty_assertions::assert_eq!(
                parse(source_code, config).format(0, false).0 + "\n",
                source_code
            )
        }},

        ($source_code:expr, $expected:expr $(,)?) => {{
            let config = &Config {
                max_choice_nest_level: 100,
            };
            pretty_assertions::assert_eq!(
                parse($source_code, config).format(0, false).0 + "\n",
                $expected
            )
        }},
    }

    mod elements_node_to_apposed_layout_expr {
        use crate::parse_test::assert_parse;
        use indoc::indoc;

        #[test]
        fn remove_empty_lines() {
            assert_parse!(
                indoc! {r#"
                    -

                    module

                    (

                    foo

                    ).
                "#},
                indoc! {r#"
                    -module(foo).
                "#},
            )
        }

        #[test]
        fn comments_indentation() {
            assert_parse!(indoc! {r#"
                    - % comment 1
                      % comment 2
                      module % comment 3
                             % comment 4
                             ( % comment 5
                               % comment 6
                               foo % comment 7
                                   % comment 8
                                   ).
                "#},)
        }

        #[test]
        fn line_comments_indentation() {
            assert_parse!(indoc! {r#"
                    - %% line comment 1
                      %% line comment 2
                      module %% line comment 3
                             %% line comment 4
                             ( %% line comment 5
                               %% line comment 6
                               foo %% line comment 7
                                   %% line comment 8
                                   ).
                "#},);
        }
    }

    mod elements_node_to_layout_expr {
        use crate::parse_test::assert_parse;
        use indoc::indoc;

        #[test]
        fn keep_empty_lines_expect_beginning_and_ending() {
            assert_parse!(
                indoc! {r#"
                    -export([

                             foo/1,

                             bar/2,

                             foobar/3,

                             baz/4

                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1,

                             bar/2,

                             foobar/3,

                             baz/4
                            ])
                "#},
            );
        }

        #[test]
        fn comments_indentation() {
            assert_parse!(indoc! {r#"
                    -export([
                             % comment 1
                             % comment 2
                             foo/1, % comment 3
                                    % comment 4
                             bar/2, % comment 7
                                    % comment 6
                             foobar/3, % comment 7
                                       % comment 8
                             baz/4 % comment 9
                                   % comment 10
                            ])
                "#},)
        }

        #[test]
        fn line_comments_indentation() {
            assert_parse!(indoc! {r#"
                    -export([
                             %% line comment 1
                             foo/1,
                             %% line comment 2
                             bar/2,
                             %% line comment 3
                             foobar/3,
                             %% line comment 4
                             baz/4
                             %% line comment 5
                            ])
                "#},)
        }

        #[test]
        fn trailing_comma() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1,
                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_with_comment() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1, % comment
                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1 % comment
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_with_line_comment() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1,
                             %% line comment
                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1
                             %% line comment
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_with_empty_line() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1,

                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_with_extras() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1, % comment 1
                                    % comment 2
                             %% line comment 1
                             %% line comment 2

                             %% line comment 3

                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1 % comment 1
                                   % comment 2
                             %% line comment 1
                             %% line comment 2

                             %% line comment 3
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_after_comment() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1 % comment
                             ,
                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1 % comment
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_after_line_comment() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1
                             %% line comment
                             ,
                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1
                             %% line comment
                            ])
                "#},
            )
        }

        #[test]
        fn trailing_comma_after_empty_line() {
            assert_parse!(
                indoc! {r#"
                    -export([
                             foo/1

                             ,
                            ])
                "#},
                indoc! {r#"
                    -export([
                             foo/1

                            ])
                "#},
            )
        }
    }

    mod binary_expression_node_to_layout_expr {
        mod lhs_op {
            use crate::parse_test::assert_parse;
            use indoc::indoc;

            #[test]
            fn remove_empty_lines() {
                assert_parse!(
                    indoc! {r#"
                        main() ->
                            Result

                            =

                            42.
                    "#},
                    indoc! {r#"
                        main() ->
                            Result =
                                42.
                    "#},
                )
            }

            #[test]
            fn comments_indentation() {
                assert_parse!(indoc! {r#"
                        main() ->
                            Result % comment 1
                                   % comment 2
                                   =
                                % comment 3
                                % comment 4
                                42.
                    "#},)
            }

            #[test]
            fn line_comments_indentation() {
                assert_parse!(indoc! {r#"
                        main() ->
                            Result %% line comment 1
                                   %% line comment 2
                                   =
                                %% line comment 3
                                %% line comment 4
                                42.
                    "#},)
            }
        }

        mod op_rhs {
            use crate::parse_test::assert_parse;
            use indoc::indoc;

            #[test]
            fn remove_empty_lines() {
                assert_parse!(
                    indoc! {r#"
                        main() ->
                            42

                            +

                            57.
                    "#},
                    indoc! {r#"
                        main() ->
                            42
                            + 57.
                    "#},
                )
            }

            #[test]
            fn comments_indentation() {
                assert_parse!(indoc! {r#"
                        main() ->
                            42 % comment 1
                               % comment 2
                            + % comment 3
                              % comment 4
                              52.
                    "#},)
            }

            #[test]
            fn line_comments_indentation() {
                assert_parse!(indoc! {r#"
                        main() ->
                            42 %% line comment 1
                               %% line comment 2
                            + %% line comment 3
                              %% line comment 4
                              52.
                    "#},)
            }
        }
    }
}

#[cfg(test)]
mod format_test {
    use super::{format, Config};

    macro assert_format {
        ($layout_fun_config:expr, $source_code:expr $(,)?) => {{
            let source_code = $source_code;
            let config = &Config{
                max_choice_nest_level: 100
            };
            pretty_assertions::assert_eq!(
                format(source_code, config, &$layout_fun_config) + "\n",
                source_code
            )
        }},

        ($layout_fun_config:expr, $source_code:expr, $expected:expr $(,)?) => {{
            let config = &Config{
                max_choice_nest_level: 100
            };
            pretty_assertions::assert_eq!(
                format($source_code, config, &$layout_fun_config) + "\n",
                $expected
            )
        }},
    }

    mod elements_node_to_layout_expr {
        mod function_clauses {
            use crate::format_test::assert_format;
            use crate::layout_fun::Config;
            use indoc::indoc;

            #[test]
            fn stacked() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() -> ok;
                        f() -> error.
                    "#},
                )
            }

            #[test]
            fn stacked_with_comments() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() -> ok; % comment 1
                        f() -> error % comment 2
                                     .
                    "#},
                )
            }
        }

        mod export_attribute {
            use crate::format_test::assert_format;
            use crate::layout_fun::Config;
            use indoc::indoc;

            #[test]
            fn apposed() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([foo/1, bar/2, foobar/3, baz/4]).
                    "#},
                )
            }

            #[test]
            fn apposed_with_beginning_and_ending_empty_lines() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([

                                 foo/1, bar/2, foobar/3, baz/4

                                ]).
                    "#},
                    indoc! {r#"
                        -export([foo/1, bar/2, foobar/3, baz/4]).
                    "#},
                )
            }

            #[test]
            fn apposed_with_multi_line_element() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([foo/1, bar/2, foobar/ % comment
                                                       3, baz/4]).
                    "#},
                );
            }

            #[test]
            fn stacked_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 10,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([
                                 foo/1,
                                 bar/2,
                                 foobar/3,
                                 baz/4
                                ]).
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_comments() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([
                                 foo/1, % comment 1
                                 bar/2, % comment 2
                                 foobar/3, % comment 3
                                 baz/4 % comment 4
                                ]).
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_line_comments() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([
                                 %% line comment 1
                                 foo/1,
                                 %% line comment 2
                                 bar/2,
                                 %% line comment 3
                                 foobar/3,
                                 %% line comment 4
                                 baz/4
                                 %% line comment 5
                                ]).
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_empty_line() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        -export([
                                 foo/1,

                                 bar/2,

                                 foobar/3,

                                 baz/4
                                ]).
                    "#},
                )
            }
        }

        mod function_clause {
            use crate::format_test::assert_format;
            use crate::layout_fun::Config;
            use indoc::indoc;

            #[test]
            fn apposed() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() -> ok.
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 5,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() ->
                            ok.
                    "#}
                )
            }

            #[test]
            fn stacked_due_to_a_comment() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() ->
                            % comment
                            ok.
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_a_line_comment() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() ->
                            %% comment
                            ok.
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_a_multiple_elements() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        f() ->
                            foo,
                            bar.
                    "#},
                )
            }
        }

        mod function_call {
            use crate::format_test::assert_format;
            use crate::layout_fun::Config;
            use indoc::indoc;

            #[test]
            fn apposed() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function("foo", "bar").
                    "#},
                )
            }

            #[test]
            fn apposed_with_a_multi_line_element() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(42 + % comment
                                              57).
                    "#},
                )
            }

            #[test]
            fn apposed_with_a_comment() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function("foo" % comment
                                         ).
                    "#},
                )
            }

            #[test]
            fn apposed_with_stacked_elements_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 27,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function("foo",
                                         "bar",
                                         "baz",
                                         "foobar").
                    "#},
                )
            }

            #[test]
            fn apposed_with_stacked_elements_due_to_comments() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(% comment 1
                                         "foo", % comment 2
                                         "bar", % comment 3
                                         "baz" % comment 4
                                         ).
                    "#},
                )
            }

            #[test]
            fn apposed_with_stacked_elements_due_to_line_comments() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(%% comment 1
                                         "foo",
                                         %% comment 2
                                         "bar",
                                         %% comment 3
                                         "baz"
                                         %% comment 4
                                         ).
                    "#},
                )
            }

            #[test]
            fn stacked_with_apposed_elements_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 27,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(
                              "foo", "bar", "baz").
                    "#},
                )
            }

            #[test]
            fn stacked_with_stacked_elements_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 22,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(
                              "foo",
                              "bar",
                              "baz").
                    "#},
                )
            }

            #[test]
            fn stacked_with_stacked_elements_due_to_comments() {
                assert_format!(
                    Config {
                        right_margin: 27,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(
                              % comment 1
                              "foo", % comment 2
                              "bar", % comment 3
                              "baz" % comment 4
                              ).
                    "#},
                )
            }

            #[test]
            fn stacked_with_stacked_elements_due_to_line_comments() {
                assert_format!(
                    Config {
                        right_margin: 27,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            mod:function(
                              %% line comment 1
                              "foo",
                              %% line comment 2
                              "bar",
                              %% line comment 3
                              "baz"
                              %% line comment 4
                              ).
                    "#},
                )
            }
        }
    }

    mod binary_expression_node_to_layout_expr {
        mod lhs_op {
            use crate::format_test::assert_format;
            use crate::layout_fun::Config;
            use indoc::indoc;

            #[test]
            fn apposed() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            Result = 3141592.
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 20,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            Result =
                                3141592.
                    "#},
                )
            }
        }

        mod op_rhs {
            use crate::format_test::assert_format;
            use crate::layout_fun::Config;
            use indoc::indoc;

            #[test]
            fn apposed() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            271828 + 3141592.
                    "#},
                )
            }

            #[test]
            fn stacked_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 20,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                    },
                    indoc! {r#"
                        main() ->
                            %% line comment
                            271828
                            + 3141592.
                    "#},
                )
            }
        }
    }
}
