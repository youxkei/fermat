#![feature(box_syntax, box_patterns, bindings_after_at, decl_macro)]

mod avltree;
mod layout_expr;
mod layout_fun;

use std::fs;
use std::iter::repeat;
use std::rc::Rc;

use tree_sitter::{Language, Node, Parser};

use layout_expr::{apposition, choice, height_cost, stack, text, unit, LayoutExpr};
use layout_fun::{Config, LayoutFun};

extern "C" {
    fn tree_sitter_erlang() -> Language;
}

tree_sitter_id::define_kind_id! {}

fn main() {
    let source_code = fs::read_to_string("hello.erl").unwrap();

    let config = Config {
        right_margin: 80,
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
        KindId::SOURCE_FILE => {
            let mut result = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                result = stack!(result, node_to_layout_expr(child, source_code))
            }

            result
        }

        KindId::FORM => {
            let mut result = unit!();
            let mut after_attribute_or_function = false;

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::MULTIPLE_NEWLINES => {}

                    KindId::COMMENT => {
                        result = stack!(
                            apposition!(
                                result,
                                if after_attribute_or_function {
                                    text!(" ")
                                } else {
                                    unit!()
                                },
                                node_to_layout_expr(child, source_code)
                            ),
                            text!("")
                        );

                        after_attribute_or_function = false;
                    }

                    KindId::LINE_COMMENT => {
                        result = stack!(result, node_to_layout_expr(child, source_code), text!(""));

                        after_attribute_or_function = false;
                    }

                    KindId::MODULE_ATTRIBUTE | KindId::EXPORT_ATTRIBUTE | KindId::FUNCTION => {
                        result = node_to_layout_expr(child, source_code);

                        after_attribute_or_function = true;
                    }

                    KindId::DOT => {
                        result = apposition!(result, node_to_layout_expr(child, source_code));

                        after_attribute_or_function = false;
                    }

                    _ => panic!("{:?} should not have {:?}", node, child),
                }
            }

            result
        }

        KindId::MODULE_ATTRIBUTE => {
            let mut result = unit!();
            let mut comments = unit!();

            let mut cursor = node.walk();

            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::MULTIPLE_NEWLINES => {}

                    KindId::COMMENT | KindId::LINE_COMMENT => {
                        comments = stack!(comments, node_to_layout_expr(child, source_code))
                    }

                    KindId::DASH
                    | KindId::MODULE
                    | KindId::LPAREN
                    | KindId::ATOM
                    | KindId::RPAREN => {
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

                    _ => panic!("{:?} should not have {:?}", node, child),
                }
            }

            result
        }

        KindId::EXPORT_ATTRIBUTE => {
            let mut before_bracket = unit!();
            let mut in_bracket_elements = vec![];
            let mut after_bracket = unit!();

            let mut comments = unit!();
            let mut element = unit!();
            let mut comment_or_empty_line_in_bracket = false;

            enum Phase {
                BeforeBracket,
                InBracket,
                AfterBracket,
            }
            let mut phase = Phase::BeforeBracket;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match phase {
                    Phase::BeforeBracket => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT | KindId::LINE_COMMENT => {
                            comments = stack!(comments, node_to_layout_expr(child, source_code))
                        }

                        KindId::DASH | KindId::EXPORT | KindId::LPAREN => {
                            before_bracket = apposition!(
                                before_bracket,
                                if comments == unit!() {
                                    unit!()
                                } else {
                                    text!(" ")
                                },
                                stack!(comments, node_to_layout_expr(child, source_code))
                            );

                            comments = unit!();
                        }

                        KindId::LBRACK => {
                            if comments != unit!() {
                                before_bracket = apposition!(
                                    before_bracket,
                                    text!(" "),
                                    stack!(comments, text!(""))
                                );
                            }

                            comments = unit!();

                            phase = Phase::InBracket;
                        }

                        _ => panic!("{:?} should not have {:?}", node, child),
                    },

                    Phase::InBracket => match kind_id(child) {
                        KindId::COMMA => {
                            element = apposition!(element, node_to_layout_expr(child, source_code));
                        }

                        KindId::COMMENT => {
                            if element == unit!() {
                                element = node_to_layout_expr(child, source_code);
                            } else {
                                element = apposition!(
                                    element,
                                    text!(" "),
                                    node_to_layout_expr(child, source_code)
                                );
                            }

                            in_bracket_elements.push(element);
                            element = unit!();

                            comment_or_empty_line_in_bracket = true;
                        }

                        KindId::MULTIPLE_NEWLINES | KindId::LINE_COMMENT => {
                            in_bracket_elements.push(element);
                            element = node_to_layout_expr(child, source_code);

                            comment_or_empty_line_in_bracket = true;
                        }

                        KindId::EXPORT_MFA => {
                            in_bracket_elements.push(element);
                            element = node_to_layout_expr(child, source_code);
                        }

                        KindId::RBRACK => {
                            if element != text!("") {
                                in_bracket_elements.push(element);
                            }
                            element = unit!();

                            phase = Phase::AfterBracket;
                        }

                        _ => panic!("{:?} should not have {:?}", node, child),
                    },

                    Phase::AfterBracket => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT | KindId::LINE_COMMENT => {
                            comments = stack!(comments, node_to_layout_expr(child, source_code))
                        }

                        KindId::RPAREN => {
                            after_bracket = apposition!(
                                after_bracket,
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
                }
            }

            let left_bracket = text!("[");
            let right_bracket = text!("]");

            let mut stacked_mfas = unit!();
            let mut apposed_mfas = unit!();

            for element in in_bracket_elements {
                stacked_mfas = stack!(stacked_mfas, element.clone());

                if apposed_mfas == unit!() {
                    apposed_mfas = height_cost!(element);
                } else {
                    apposed_mfas = apposition!(apposed_mfas, text!(" "), height_cost!(element));
                }
            }

            let stack_style = apposition!(
                before_bracket.clone(),
                stack!(
                    left_bracket.clone(),
                    apposition!(text!(" "), stacked_mfas),
                    right_bracket.clone(),
                ),
                after_bracket.clone(),
            );

            if comment_or_empty_line_in_bracket {
                stack_style
            } else {
                let apposition_style = apposition!(
                    before_bracket,
                    left_bracket,
                    apposed_mfas,
                    right_bracket,
                    after_bracket
                );

                choice!(stack_style, apposition_style)
            }
        }

        KindId::EXPORT_MFA => {
            let mut result = unit!();
            let mut comments = unit!();

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::MULTIPLE_NEWLINES => {}

                    KindId::COMMENT | KindId::LINE_COMMENT => {
                        comments = stack!(comments, node_to_layout_expr(child, source_code))
                    }

                    KindId::ATOM | KindId::SLASH | KindId::INTEGER => {
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

                    _ => panic!("{:?} should not have {:?}", node, child),
                }
            }

            result
        }

        KindId::FUNCTION => {
            let mut result = unit!();
            let mut line = unit!();
            let mut after_semi = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMENT => {
                        if after_semi {
                            line = apposition!(
                                line,
                                text!(" "),
                                node_to_layout_expr(child, source_code)
                            );
                        } else {
                            result = stack!(result, line);
                            line = node_to_layout_expr(child, source_code);
                        };

                        after_semi = false;
                    }

                    KindId::SEMI => {
                        line = apposition!(line, node_to_layout_expr(child, source_code));
                        after_semi = true;
                    }

                    KindId::FUNCTION_CLAUSE | KindId::LINE_COMMENT | KindId::MULTIPLE_NEWLINES => {
                        result = stack!(result, line);
                        line = node_to_layout_expr(child, source_code);

                        after_semi = false;
                    }

                    _ => panic!("{:?} should not have {:?}", node, child),
                }
            }

            stack!(result, line)
        }

        KindId::FUNCTION_CLAUSE => {
            let mut before_body = unit!();
            let mut body = unit!();

            enum Phase {
                BeforeDashGt,
                AfterDashGt,
                InBody,
            }
            let mut phase = Phase::BeforeDashGt;
            let mut comments = unit!();
            let mut comment_after_dash_gt = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match phase {
                    Phase::BeforeDashGt => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT | KindId::LINE_COMMENT => {
                            comments = stack!(comments, node_to_layout_expr(child, source_code))
                        }

                        KindId::DASH_GT => {
                            before_body = apposition!(
                                before_body,
                                text!(" "),
                                stack!(comments, node_to_layout_expr(child, source_code)),
                            );

                            comments = unit!();
                            phase = Phase::AfterDashGt;
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

                    Phase::AfterDashGt => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT => {
                            before_body = apposition!(
                                before_body,
                                text!(" "),
                                node_to_layout_expr(child, source_code)
                            );
                            phase = Phase::InBody;
                            comment_after_dash_gt = true;
                        }

                        KindId::LINE_COMMENT | KindId::EXPRS => {
                            body = stack!(body, node_to_layout_expr(child, source_code));
                            phase = Phase::InBody;
                        }

                        _ => panic!("{:?} should not have {:?}", node, child),
                    },

                    Phase::InBody => match kind_id(child) {
                        KindId::MULTIPLE_NEWLINES => {}

                        KindId::COMMENT | KindId::LINE_COMMENT | KindId::EXPRS => {
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
                    apposition!(before_body, text!(" "), height_cost!(body)),
                )
            }
        }

        KindId::EXPRS => {
            let mut result = unit!();
            let mut line = unit!();
            let mut after_comma = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::COMMA => {
                        line = apposition!(line, node_to_layout_expr(child, source_code));

                        after_comma = true;
                    }

                    KindId::COMMENT => {
                        if after_comma {
                            line = apposition!(
                                line,
                                text!(" "),
                                node_to_layout_expr(child, source_code)
                            );
                        } else {
                            result = stack!(result, line);
                            line = node_to_layout_expr(child, source_code);
                        }

                        after_comma = false;
                    }

                    KindId::LINE_COMMENT | KindId::MULTIPLE_NEWLINES | KindId::EXPR => {
                        result = stack!(result, line);
                        line = node_to_layout_expr(child, source_code);

                        after_comma = false;
                    }

                    _ => panic!("{:?} should not have {:?}", node, child),
                }
            }

            stack!(result, line)
        }

        KindId::STRINGS => {
            let mut result = unit!();
            let mut line = unit!();
            let mut after_string = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match kind_id(child) {
                    KindId::MULTIPLE_NEWLINES => {}

                    KindId::COMMENT => {
                        if after_string {
                            line = apposition!(
                                line,
                                text!(" "),
                                node_to_layout_expr(child, source_code)
                            );
                        } else {
                            result = stack!(result, line);
                            line = node_to_layout_expr(child, source_code);
                        }

                        after_string = false;
                    }

                    KindId::LINE_COMMENT => {
                        result = stack!(result, line);
                        line = node_to_layout_expr(child, source_code);

                        after_string = false;
                    }

                    KindId::STRING => {
                        result = stack!(result, line);
                        line = node_to_layout_expr(child, source_code);

                        after_string = true;
                    }

                    _ => panic!("{:?} should not have {:?}", node, child),
                };
            }

            stack!(result, line)
        }

        KindId::PAT_ARGUMENT_LIST
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
        | KindId::RBRACK
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

        KindId::ERROR => panic!("syntax error {:?}", node),
    }
}

fn kind_id(node: Node<'_>) -> KindId {
    unsafe { std::mem::transmute(node.kind_id()) }
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
    fn form() {
        assert_parse! {
            "remove newlines",
            indoc! {r#"
                -module(foo)

                .
            "#},
            indoc! {r#"
                -module(foo).
            "#},
        }

        assert_parse! {
            "remove empty lines",
            indoc! {r#"
                -module(foo)

                %% line comment

                .
            "#},
            indoc! {r#"
                -module(foo)
                %% line comment
                .
            "#},
        }

        assert_parse! {
            "there can be only one comment after attribute",
            indoc! {r#"
                -module(foo) % comment 1
                             % comment 2
                .
            "#},
            indoc! {r#"
                -module(foo) % comment 1
                % comment 2
                .
            "#},
        }

        assert_parse! {
            "there can be no line comments after attribute",
            indoc! {r#"
                -module(foo) %% line comment
                .
            "#},
            indoc! {r#"
                -module(foo)
                %% line comment
                .
            "#},
        }
    }

    #[test]
    fn module_attribute() {
        assert_parse! {
            "remove empty lines",
            indoc! {r#"
                -

                module

                (

                foo

                )

                .
            "#},
            indoc! {r#"
                -module(foo).
            "#},
        }

        assert_parse! {
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
        }

        assert_parse! {
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
        }
    }

    #[test]
    fn export_attribute() {
        assert_parse!(
            "keep empty lines in body",
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
    fn function() {
        assert_parse! {
            "keep empty lines",
            indoc! {r#"
                f() ->
                    foo;

                %% line comment

                f() ->
                    bar.
            "#},
        }

        assert_parse! {
            "there can be only one comment after ';'",
            indoc! {r#"
                f() ->
                    foo; % comment 1
                         % comment 2

                f() ->
                    bar.
            "#},
            indoc! {r#"
                f() ->
                    foo; % comment 1
                % comment 2

                f() ->
                    bar.
            "#},
        }

        assert_parse! {
            "there can be no line comments after ';'",
            indoc! {r#"
                f() ->
                    foo; %% line comment

                f() ->
                    bar.
            "#},
            indoc! {r#"
                f() ->
                    foo;
                %% line comment

                f() ->
                    bar.
            "#},
        }
    }

    #[test]
    fn function_clause() {
        /* before '->' */
        {
            assert_parse! {
                "remove empty lines",
                indoc! {r#"
                    main

                    ()

                    -> io:format("Hello, world!").
                "#},
                indoc! {r#"
                    main() ->
                        io:format("Hello, world!").
                "#},
            }

            assert_parse! {
                "comment indentation",
                indoc! {r#"
                    main % comment 1
                    % comment 2
                    () % comment 3
                    % comment 4
                    -> io:format("Hello, world!").
                "#},
                indoc! {r#"
                    main % comment 1
                         % comment 2
                         () % comment 3
                            % comment 4
                            ->
                        io:format("Hello, world!").
                "#},
            }

            assert_parse! {
                "line comments behave like comments",
                indoc! {r#"
                    main %% line comment 1
                    %% line comment 2
                    () %% line comment 3
                    %% line comment 4
                    -> io:format("Hello, world!").
                "#},
                indoc! {r#"
                    main %% line comment 1
                         %% line comment 2
                         () %% line comment 3
                            %% line comment 4
                            ->
                        io:format("Hello, world!").
                "#},
            }
        }

        /* between '->' and body */
        {
            assert_parse! {
                "remove empty lines",
                indoc! {r#"
                    main() ->

                        %% line comment

                        io:format("Hello, world!").
                "#},
                indoc! {r#"
                    main() ->
                        %% line comment
                        io:format("Hello, world!").
                "#},
            }

            assert_parse! {
                "there can be only one comment after '->'",
                indoc! {r#"
                    main() -> % comment 1
                              % comment 2
                        io:format("Hello, world!").
                "#},
                indoc! {r#"
                    main() -> % comment 1
                        % comment 2
                        io:format("Hello, world!").
                "#},
            }

            assert_parse! {
                "there can be no line comments after '->'",
                indoc! {r#"
                    main() -> %% comment
                        io:format("Hello, world!").
                "#},
                indoc! {r#"
                    main() ->
                        %% comment
                        io:format("Hello, world!").
                "#},
            }
        }
    }

    #[test]
    fn exprs() {
        assert_parse! {
            "keep empty lines",
            indoc! {r#"
                f() ->
                    foo,

                    %% line comment

                    bar.
            "#},
        }

        assert_parse! {
            "there can be only one comment after ','",
            indoc! {r#"
                f() ->
                    foo, % comment 1
                         % comment 2

                    bar.
            "#},
            indoc! {r#"
                f() ->
                    foo, % comment 1
                    % comment 2

                    bar.
            "#},
        }

        assert_parse! {
            "there can be no line comments after ','",
            indoc! {r#"
                f() ->
                    foo, %% line comment

                    bar.
            "#},
            indoc! {r#"
                f() ->
                    foo,
                    %% line comment

                    bar.
            "#},
        }
    }

    #[test]
    fn strings() {
        assert_parse! {
            "always stacks",
            indoc! {r#"
                f() ->
                    "foo" "bar".
            "#},
            indoc! {r#"
                f() ->
                    "foo"
                    "bar".
            "#},
        }

        assert_parse! {
            "remove empty lines",
            indoc! {r#"
                f() ->
                    "foo"

                    %% line comment

                    "bar".
            "#},
            indoc! {r#"
                f() ->
                    "foo"
                    %% line comment
                    "bar".
            "#},
        }

        assert_parse! {
            "there can be only one comment after string",
            indoc! {r#"
                f() ->
                    "foo" % comment 1
                          % comment 2
                    "bar".
            "#},
            indoc! {r#"
                f() ->
                    "foo" % comment 1
                    % comment 2
                    "bar".
            "#},
        }

        assert_parse! {
            "there can be no line comments after ','",
            indoc! {r#"
                f() ->
                    "foo" %% line comment

                    "bar".
            "#},
            indoc! {r#"
                f() ->
                    "foo"
                    %% line comment
                    "bar".
            "#},
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
    fn export_attribute() {
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
            "stack style due to comments",
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
            "stack style due to comments",
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
            "stack style due to empty lines",
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
    fn function_clause() {
        assert_format!(
            "apposition style",
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
            "stack style",
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
            "stack style with comment",
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
            "stack style with line comment",
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
            "stack style with multi line body",
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
