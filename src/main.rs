#![feature(box_syntax, box_patterns, bindings_after_at, decl_macro)]

mod avltree;
mod layout_expr;
mod layout_fun;

use std::fs;
use std::iter::repeat;
use std::rc::Rc;

use tree_sitter::{Language, Node, Parser};

use layout_expr::{apposition, choice, multi_line_cost, stack, text, unit, LayoutExpr};
use layout_fun::{Config, LayoutFun};

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

tree_sitter_id::define_kind_id! {}

enum BlockStyle {
    OneSpaceWithTrailingNewline,
    OneSpaceWithoutTrailingNewline,
    TweSpaceWithoutTrailingNewline,
}

fn main() {
    let source_code = fs::read_to_string("hello.erl").unwrap();

    let config = Config {
        right_margin: 49,
        newline_cost: 1,
        beyond_right_margin_cost: 10000,
        height_cost: 100,
    };

    println!(
        "{}|\n{}",
        repeat(' ')
            .take((config.right_margin - 1) as usize)
            .collect::<String>(),
        format(&source_code, &config),
    );
}

fn format(source_code: &str, config: &Config) -> String {
    let layout_expr = parse(&source_code);
    let layout_fun = LayoutFun::from_layout_expr(&*layout_expr, &config);

    layout_fun.at(0).layout_expr.format(0, false).0
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
        KindId::FUNCTION_CLAUSE => {
            let mut before_body = unit!();
            let mut body = unit!();

            enum Phase {
                BeforeHyphenGt,
                AfterHyphenGt,
                InBody,
            }
            let mut phase = Phase::BeforeHyphenGt;
            let mut comments = unit!();
            let mut comment_after_dash_gt = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match phase {
                    Phase::BeforeHyphenGt => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT | KindId::LINE_COMMENT => {
                            comments = stack!(comments, node_to_layout_expr(child, source_code))
                        }

                        KindId::HYPHEN_GT => {
                            before_body = apposition!(
                                before_body,
                                text!(" "),
                                stack!(comments, node_to_layout_expr(child, source_code)),
                            );

                            comments = unit!();
                            phase = Phase::AfterHyphenGt;
                        }

                        KindId::ATOM | KindId::PAT_ARGUMENT_LIST | KindId::CLAUSE_GUARD => {
                            before_body = apposition!(
                                before_body,
                                if comments == unit!() {
                                    unit!()
                                } else {
                                    text!(" ")
                                },
                                stack!(comments, node_to_layout_expr(child, source_code))
                            );

                            comments = unit!();
                        }

                        _ => panic!("{:?} should not have {:?}", node, child),
                    },

                    Phase::AfterHyphenGt => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT => {
                            comments = stack!(comments, node_to_layout_expr(child, source_code));

                            comment_after_dash_gt = true;
                        }

                        KindId::LINE_COMMENT | KindId::FUNCTION_CLAUSE_EXPRS_TRAILING_COMMA => {
                            if comments != unit!() {
                                before_body = apposition!(before_body, text!(" "), comments);
                            }
                            comments = unit!();

                            body = node_to_layout_expr(child, source_code);
                            phase = Phase::InBody;
                        }

                        _ => panic!("{:?} should not have {:?}", node, child),
                    },

                    Phase::InBody => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT
                        | KindId::LINE_COMMENT
                        | KindId::FUNCTION_CLAUSE_EXPRS_TRAILING_COMMA => {
                            body = stack!(body, node_to_layout_expr(child, source_code));
                        }

                        _ => panic!("{:?} should not have {:?}", node, child),
                    },
                }
            }

            if comment_after_dash_gt {
                stack!(before_body, apposition!(text!("    "), body))
            } else {
                choice!(
                    stack!(
                        before_body.clone(),
                        apposition!(text!("    "), body.clone())
                    ),
                    apposition!(before_body, text!(" "), multi_line_cost!(body)),
                )
            }
        }

        // elements that should be apposed
        KindId::FORM
        | KindId::MODULE_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE_MFA
        | KindId::PAT_ARGUMENT_LIST
        | KindId::CLAUSE_GUARD
        | KindId::EXPR
        | KindId::FUNCTION_CALL_OPEN
        | KindId::EXPR_REMOTE
        | KindId::EXPR_MAX => {
            let mut result = unit!();
            let mut comments = unit!();

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::MULTIPLE_NEWLINES => {}

                    KindId::COMMENT | KindId::LINE_COMMENT => {
                        comments = stack!(comments, node_to_layout_expr(child, source_code))
                    }

                    _ => {
                        result = apposition!(
                            result,
                            if comments == unit!() {
                                unit!()
                            } else {
                                text!(" ")
                            },
                            stack!(comments, node_to_layout_expr(child, source_code))
                        );

                        comments = unit!();
                    }
                }
            }

            result
        }

        // elements that should be stacked or apposed
        KindId::SOURCE_FILE
        | KindId::FUNCTION_CLAUSES
        | KindId::EXPRS
        | KindId::FUNCTION_CLAUSE_EXPRS
        | KindId::EXPORT_ATTRIBUTE_MFAS
        | KindId::STRINGS => {
            let mut elements = vec![];
            let mut element = unit!();
            let mut comments = unit!();
            let mut apposable = false;
            let mut should_stack = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMENT => {
                        comments = stack!(comments, node_to_layout_expr(child, source_code));

                        should_stack = true;
                    }

                    KindId::COMMA | KindId::SEMICOLON | KindId::PERIOD => {
                        if apposable {
                            if comments.is_unit() {
                                element =
                                    apposition!(element, node_to_layout_expr(child, source_code));
                            } else {
                                element = apposition!(element, text!(" "), comments);
                                elements.push(element);
                                comments = unit!();

                                element = node_to_layout_expr(child, source_code);
                            }
                        } else {
                            elements.push(element);
                            elements.push(comments);
                            comments = unit!();

                            element = node_to_layout_expr(child, source_code);
                        }

                        apposable = true;
                    }

                    kind_id => {
                        if apposable {
                            if !comments.is_unit() {
                                element = apposition!(element, text!(" "), comments);
                            }

                            elements.push(element);
                        } else {
                            elements.push(element);
                            elements.push(comments);
                        }
                        comments = unit!();

                        element = node_to_layout_expr(child, source_code);

                        apposable = is_apposable(kind_id);
                        should_stack |= !apposable;
                    }
                }
            }

            if apposable {
                if !comments.is_unit() {
                    element = apposition!(element, text!(" "), comments);
                }

                elements.push(element);
            } else {
                elements.push(element);
                elements.push(comments);
            }

            let mut stacked_elements = unit!();
            let mut apposed_elements = unit!();

            for element in elements {
                stacked_elements = stack!(stacked_elements, element.clone());

                if apposed_elements == unit!() {
                    apposed_elements = multi_line_cost!(element);
                } else {
                    apposed_elements =
                        apposition!(apposed_elements, text!(" "), multi_line_cost!(element));
                }
            }

            if should_stack || self::should_stack(node) {
                stacked_elements
            } else {
                choice!(stacked_elements, apposed_elements)
            }
        }

        // trailing separator
        KindId::FUNCTION_CLAUSES_TRAILING_SEMICOLON
        | KindId::FUNCTION_CLAUSE_EXPRS_TRAILING_COMMA => {
            let mut comments = unit!();
            let mut body = unit!();
            let mut line = unit!();

            let mut apposable = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMA | KindId::SEMICOLON => {}

                    KindId::COMMENT => {
                        comments = stack!(comments, node_to_layout_expr(child, source_code));
                    }

                    kind_id => {
                        if apposable {
                            if line != unit!() && comments != unit!() {
                                line = apposition!(line, text!(" "), comments);
                            } else {
                                line = apposition!(line, comments);
                            }
                        } else {
                            line = stack!(line, comments)
                        }
                        comments = unit!();

                        body = stack!(body, line);
                        line = node_to_layout_expr(child, source_code);

                        apposable = is_apposable(kind_id);
                    }
                }
            }

            if !line.is_empty() {
                body = stack!(body, line);
            }

            body
        }

        // block
        KindId::EXPORT_ATTRIBUTE_BLOCK | KindId::FUNCTION_CALL_BLOCK => {
            let mut comments = unit!();
            let mut open = unit!();
            let mut close = unit!();
            let mut body = unit!();
            let mut line = unit!();

            let mut after_open = false;
            let mut apposable = false;
            let mut should_stack = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMA | KindId::SEMICOLON => {}

                    KindId::COMMENT => {
                        comments = stack!(comments, node_to_layout_expr(child, source_code));
                        should_stack = true
                    }

                    kind_id => {
                        if kind_id == KindId::MULTIPLE_NEWLINES && after_open {
                            after_open = false;

                            continue;
                        }

                        if kind_id.is_open() {
                            open = node_to_layout_expr(child, source_code);
                            after_open = true;

                            continue;
                        }

                        if apposable {
                            if line != unit!() && comments != unit!() {
                                line = apposition!(line, text!(" "), comments);
                            } else {
                                line = apposition!(line, comments);
                            }
                        } else {
                            line = stack!(line, comments)
                        }
                        comments = unit!();

                        if kind_id.is_close() {
                            if !line.is_empty() {
                                body = stack!(body, line);
                            }
                            line = unit!();

                            close = node_to_layout_expr(child, source_code);
                        } else {
                            body = stack!(body, line);
                            line = node_to_layout_expr(child, source_code);

                            apposable = is_apposable(kind_id);
                            should_stack |= !apposable
                        }
                    }
                }
            }

            let stacked = match block_style(node) {
                BlockStyle::OneSpaceWithTrailingNewline => stack!(
                    open.clone(),
                    apposition!(text!(" "), body.clone()),
                    close.clone()
                ),

                BlockStyle::OneSpaceWithoutTrailingNewline => stack!(
                    open.clone(),
                    apposition!(text!(" "), body.clone(), close.clone()),
                ),

                BlockStyle::TweSpaceWithoutTrailingNewline => stack!(
                    open.clone(),
                    apposition!(text!("  "), body.clone(), close.clone()),
                ),
            };

            if should_stack {
                stacked
            } else {
                if deny_multi_line_body_apposed(node) {
                    body = multi_line_cost!(body)
                }

                choice!(stacked, apposition!(open, body, close),)
            }
        }

        // text
        KindId::HYPHEN_GT
        | KindId::PAREN_OPEN
        | KindId::PAREN_CLOSE
        | KindId::BRACKET_OPEN
        | KindId::BRACKET_CLOSE
        | KindId::COMMA
        | KindId::PERIOD
        | KindId::HYPHEN
        | KindId::SLASH
        | KindId::SEMICOLON
        | KindId::COLON
        | KindId::DOUBLE_QUOTE
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

        KindId::ERROR => panic!("syntax error {:?}", node),
    }
}

fn kind_id(node: Node<'_>) -> KindId {
    unsafe { std::mem::transmute(node.kind_id()) }
}

fn block_style(node: Node<'_>) -> BlockStyle {
    match kind_id(node) {
        KindId::EXPORT_ATTRIBUTE_BLOCK => BlockStyle::OneSpaceWithTrailingNewline,
        KindId::FUNCTION_CALL_BLOCK => BlockStyle::TweSpaceWithoutTrailingNewline,
        _ => panic!("{:?} is not a block node", node),
    }
}

fn deny_multi_line_body_apposed(node: Node<'_>) -> bool {
    match kind_id(node) {
        KindId::EXPORT_ATTRIBUTE_BLOCK => true,
        _ => false,
    }
}

fn is_apposable(kind_id: KindId) -> bool {
    match kind_id {
        KindId::MULTIPLE_NEWLINES | KindId::COMMENT | KindId::LINE_COMMENT => false,
        _ => true,
    }
}

fn should_stack(node: Node<'_>) -> bool {
    match kind_id(node) {
        KindId::SOURCE_FILE => true,
        KindId::FUNCTION_CLAUSES => true,
        KindId::FUNCTION_CLAUSE_EXPRS => true,
        KindId::STRINGS => true,
        _ => false,
    }
}

#[cfg(test)]
mod parse_test {
    use super::parse;
    use indoc::indoc;

    macro_rules! assert_parse {
        ($desc:literal, $source_code:expr $(,)?) => {{
            let source_code = $source_code;
            pretty_assertions::assert_eq!(
                parse(source_code).format(0, false).0 + "\n",
                source_code,
                $desc
            )
        }};

        ($desc:literal, $source_code:expr, $expected:expr $(,)?) => {{
            pretty_assertions::assert_eq!(
                parse($source_code).format(0, false).0 + "\n",
                $expected,
                $desc
            )
        }};
    }

    #[test]
    fn elements_that_should_be_apposed() {
        assert_parse!(
            "remove empty lines",
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
        );

        assert_parse!(
            "comments indentation",
            indoc! {r#"
                - % comment 1
                  % comment 2
                  module % comment 3
                         % comment 4
                         ( % comment 5
                           % comment 6
                           foo % comment 7
                               % comment 8
                               ).
            "#},
        );

        assert_parse!(
            "line comments indentation",
            indoc! {r#"
                - %% line comment 1
                  %% line comment 2
                  module %% line comment 3
                         %% line comment 4
                         ( %% line comment 5
                           %% line comment 6
                           foo %% line comment 7
                               %% line comment 8
                               ).
            "#},
        );
    }

    #[test]
    fn elements_that_should_be_stacked_or_apposed() {
        assert_parse!(
            "keep empty lines",
            indoc! {r#"
                -export([
                         foo/1,

                         bar/2,

                         foobar/3,

                         baz/4
                        ])
            "#},
        );

        assert_parse!(
            "comments indentation",
            indoc! {r#"
                -export([
                         foo/1, % comment 1
                                % comment 2
                         bar/2, % comment 3
                                % comment 4
                         foobar/3, % comment 5
                                   % comment 6
                         baz/4 % comment 7
                               % comment 8
                        ])
            "#},
        );

        assert_parse!(
            "there can be no line comment after an element",
            indoc! {r#"
                -export([
                         foo/1, %% line comment 1
                         bar/2, %% line comment 2
                         foobar/3, %% line comment 3
                         baz/4 %% line comment 4
                        ])
            "#},
            indoc! {r#"
                -export([
                         foo/1,
                         %% line comment 1
                         bar/2,
                         %% line comment 2
                         foobar/3,
                         %% line comment 3
                         baz/4
                         %% line comment 4
                        ])
            "#},
        );
    }

    #[test]
    fn block() {
        assert_parse!(
            "remove empty lines",
            indoc! {r#"
                -export([

                         main/0

                         ]).
            "#},
            indoc! {r#"
                -export([
                         main/0
                        ]).
            "#},
        );

        assert_parse!(
            "comments indentation",
            indoc! {r#"
                -export([
                         % comment 1
                         % comment 2
                         main/0 % comment 3
                                % comment 4
                        ]).
            "#},
        );

        assert_parse!(
            "line comments indentation",
            indoc! {r#"
                -export([
                         %% comment 1
                         main/0
                         %% comment 2
                        ]).
            "#},
        );

        assert_parse!(
            "remove trailing separator",
            indoc! {r#"
                -export([
                         % comment 1
                         % comment 2
                         main/0 % comment 3
                         , % comment 4
                        ]).
            "#},
            indoc! {r#"
                -export([
                         % comment 1
                         % comment 2
                         main/0 % comment 3
                                % comment 4
                        ]).
            "#},
        );
    }

    #[test]
    fn function_clause() {
        /* before '->' */
        {
            assert_parse!(
                "remove empty lines",
                indoc! {r#"
                    main

                    ()

                    -> ok.
                "#},
                indoc! {r#"
                    main() ->
                        ok.
                "#},
            );

            assert_parse!(
                "comment indentation",
                indoc! {r#"
                    main % comment 1
                    % comment 2
                    () % comment 3
                    % comment 4
                    -> ok.
                "#},
                indoc! {r#"
                    main % comment 1
                         % comment 2
                         () % comment 3
                            % comment 4
                            ->
                        ok.
                "#},
            );

            assert_parse!(
                "line comments behave like comments",
                indoc! {r#"
                    main %% line comment 1
                    %% line comment 2
                    () %% line comment 3
                    %% line comment 4
                    -> ok.
                "#},
                indoc! {r#"
                    main %% line comment 1
                         %% line comment 2
                         () %% line comment 3
                            %% line comment 4
                            ->
                        ok.
                "#},
            );
        }

        /* between '->' and body */
        {
            assert_parse!(
                "remove empty lines",
                indoc! {r#"
                    main() ->

                        %% line comment

                        ok.
                "#},
                indoc! {r#"
                    main() ->
                        %% line comment
                        ok.
                "#},
            );

            assert_parse!(
                "comment indentation",
                indoc! {r#"
                    main() -> % comment 1
                              % comment 2
                        ok.
                "#},
            );

            assert_parse!(
                "there can be no line comments after '->'",
                indoc! {r#"
                    main() -> %% comment
                        ok.
                "#},
                indoc! {r#"
                    main() ->
                        %% comment
                        ok.
                "#},
            );
        }
    }
}

#[cfg(test)]
mod format_test {
    use super::format;
    use super::layout_fun::Config;
    use indoc::indoc;

    macro_rules! assert_format {
        ($desc:literal, $config:expr, $source_code:expr $(,)?) => {{
            let source_code = $source_code;
            pretty_assertions::assert_eq!(format(source_code, &$config) + "\n", source_code, $desc)
        }};

        ($desc:literal, $config:expr, $source_code:expr, $expected:expr $(,)?) => {{
            pretty_assertions::assert_eq!(format($source_code, &$config) + "\n", $expected, $desc)
        }};
    }

    #[test]
    fn elements_that_should_be_stacked_or_apposed() {
        assert_format!(
            "apposed",
            Config {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([foo/1, bar/2, foobar/3, baz/4]).
            "#},
        );

        assert_format!(
            "stacked due to right margin",
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
        );

        assert_format!(
            "stacked due to comments",
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
        );

        assert_format!(
            "stacked due to a comment",
            Config {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([
                         foo/1 % comment 1
                        ]).
            "#},
        );

        assert_format!(
            "stacked due to line comments",
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
        );

        assert_format!(
            "stacked due to empty lines",
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
        );
    }

    #[test]
    fn block_with_one_space_with_trailing_newline() {
        assert_format!(
            "apposed",
            Config {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([foo/1]).
            "#},
        );

        assert_format!(
            "stacked due to right margin",
            Config {
                right_margin: 14,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([
                         foo/1
                        ]).
            "#},
        );
    }

    #[test]
    fn block_with_two_space_without_trailing_newline() {
        assert_format!(
            "apposed",
            Config {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                f() ->
                    %% comment
                    io:format("foo").
            "#},
        );

        assert_format!(
            "stacked due to right margin",
            Config {
                right_margin: 14,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                f() ->
                    %% comment
                    io:format(
                      "foo").
            "#},
        );
    }

    #[test]
    fn function_clause() {
        assert_format!(
            "apposed",
            Config {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                f() -> ok.
            "#}
        );

        assert_format!(
            "stacked due to right margin",
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
        );

        assert_format!(
            "stacked due to a comment",
            Config {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                f() -> % comment
                    ok.
            "#}
        );

        assert_format!(
            "stacked due to a line comment",
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
            "#}
        );

        assert_format!(
            "stacked due to a multi line body",
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
            "#}
        );
    }
}
