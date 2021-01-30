#![feature(box_syntax, box_patterns, bindings_after_at, decl_macro)]

mod avltree;
mod layout_expr;
mod layout_fun;

use std::fs;
use std::rc::Rc;

use tree_sitter::{Language, Node, Parser};

use layout_expr::{apposition, choice, multi_line_cost, stack, text, unit, LayoutExpr};
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
    let layout_fun = LayoutFun::from_layout_expr(&*layout_expr, layout_fun_config);

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
        | KindId::PAT_ARGUMENT_LIST
        | KindId::CLAUSE_GUARD
        | KindId::FUNCTION_CALL_OPEN
        | KindId::UNARRY_EXPR
        | KindId::REMOTE_EXPR
        | KindId::CATCH_OP
        | KindId::EQUAL_OP
        | KindId::EXCLAM_OP
        | KindId::ADD_OP => {
            node_to_apposed_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::SOURCE_FILE | KindId::FUNCTION_CLAUSES | KindId::STRINGS => {
            node_to_stacked_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::FUNCTION_CLAUSES_TRAILING_SEMICOLON => {
            trailing_separator_node_to_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::BINARY_EXPR => {
            binary_expression_node_to_layout_expr(node, source_code, config, choice_nest_level)
        }

        KindId::EXPORT_ATTRIBUTE_BLOCK
        | KindId::FUNCTION_CLAUSE_BLOCK
        | KindId::FUNCTION_CALL_BLOCK
        | KindId::LIST => block_node_to_layout_expr(node, source_code, config, choice_nest_level),

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
        | KindId::EQUAL
        | KindId::PLUS
        | KindId::EXCLAM
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

fn node_to_apposed_layout_expr<'a>(
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

            KindId::HYPHEN_GT => {
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

fn node_to_stacked_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let mut result = unit!();
    let mut element = unit!();
    let mut comments = unit!();
    let mut apposable = false;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::COMMENT => {
                comments = stack!(
                    comments,
                    node_to_layout_expr(child, source_code, config, choice_nest_level)
                );
            }

            KindId::COMMA | KindId::SEMICOLON | KindId::PERIOD => {
                if apposable {
                    if comments.is_unit() {
                        element = apposition!(
                            element,
                            node_to_layout_expr(child, source_code, config, choice_nest_level)
                        );
                    } else {
                        element = apposition!(element, text!(" "), comments);
                        result = stack!(result, element);
                        comments = unit!();

                        element =
                            node_to_layout_expr(child, source_code, config, choice_nest_level);
                    }
                } else {
                    result = stack!(result, element, comments);
                    comments = unit!();

                    element = node_to_layout_expr(child, source_code, config, choice_nest_level);
                }

                apposable = true;
            }

            kind_id => {
                if apposable {
                    if !comments.is_unit() {
                        element = apposition!(element, text!(" "), comments);
                    }

                    result = stack!(result, element);
                } else {
                    result = stack!(result, element, comments);
                }
                comments = unit!();

                element = node_to_layout_expr(child, source_code, config, choice_nest_level);

                apposable = is_apposable(kind_id);
            }
        }
    }

    if apposable {
        if !comments.is_unit() {
            element = apposition!(element, text!(" "), comments);
        }

        result = stack!(result, element);
    } else {
        result = stack!(result, element, comments);
    }

    result
}

fn node_to_stacked_or_apposed_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let mut elements = vec![];
    let mut element = unit!();
    let mut comments = unit!();
    let mut apposable = false;
    let mut should_stack = false;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::COMMENT => {
                comments = stack!(
                    comments,
                    node_to_layout_expr(child, source_code, config, choice_nest_level + 1)
                );

                should_stack = true;
            }

            KindId::COMMA | KindId::SEMICOLON | KindId::PERIOD => {
                if apposable {
                    if comments.is_unit() {
                        element = apposition!(
                            element,
                            node_to_layout_expr(child, source_code, config, choice_nest_level + 1)
                        );
                    } else {
                        element = apposition!(element, text!(" "), comments);
                        elements.push(element);
                        comments = unit!();

                        element =
                            node_to_layout_expr(child, source_code, config, choice_nest_level + 1);
                    }
                } else {
                    elements.push(element);
                    elements.push(comments);
                    comments = unit!();

                    element =
                        node_to_layout_expr(child, source_code, config, choice_nest_level + 1);
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

                element = node_to_layout_expr(child, source_code, config, choice_nest_level + 1);

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
            apposed_elements = element;
        } else {
            apposed_elements = apposition!(apposed_elements, text!(" "), element);
        }
    }

    if should_stack || choice_nest_level > config.max_choice_nest_level {
        stacked_elements
    } else {
        choice!(stacked_elements, apposed_elements)
    }
}

fn trailing_separator_node_to_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let mut comments = unit!();
    let mut body = unit!();
    let mut line = unit!();

    let mut apposable = false;
    let mut last_line_comment = false;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::COMMA | KindId::SEMICOLON | KindId::MULTIPLE_NEWLINES => {}

            KindId::COMMENT => {
                comments = stack!(
                    comments,
                    node_to_layout_expr(child, source_code, config, choice_nest_level)
                );
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
                line = node_to_layout_expr(child, source_code, config, choice_nest_level);

                apposable = is_apposable(kind_id);
                last_line_comment = kind_id == KindId::LINE_COMMENT;
            }
        }
    }

    if comments.is_unit() {
        if last_line_comment {
            body = stack!(body, line, text!(""))
        } else {
            body = stack!(body, line);
        }
    } else {
        body = stack!(
            body,
            apposition!(line, text!(" "), stack!(comments, text!("")))
        )
    }

    body
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
        KindId::EXCLAM_OP | KindId::ADD_OP => {
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

fn block_node_to_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
    config: &Config,
    choice_nest_level: u8,
) -> Rc<LayoutExpr<'a>> {
    let kind_id = kind_id(node);

    let choice_nest = match kind_id {
        KindId::EXPORT_ATTRIBUTE_BLOCK => 1,
        KindId::FUNCTION_CLAUSE_BLOCK => 1,
        KindId::FUNCTION_CALL_BLOCK => 2,
        KindId::LIST => 1,
        _ => panic!("{:?} is not covered", kind_id),
    };

    let mut elements = vec![];

    let mut comments = unit!();
    let mut open = unit!();
    let mut close = unit!();
    let mut element = unit!();
    let mut separator = unit!();

    let mut after_open = false;
    let mut element_apposable = false;
    let mut has_non_apposable = false;
    let mut last_comment = false;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::COMMENT => {
                comments = stack!(
                    comments,
                    node_to_layout_expr(
                        child,
                        source_code,
                        config,
                        choice_nest_level + choice_nest
                    )
                );

                has_non_apposable = true;
            }

            KindId::COMMA | KindId::SEMICOLON => {
                if !comments.is_unit() {
                    if element_apposable {
                        elements.push(apposition!(element, text!(" "), comments));
                    } else {
                        elements.push(element);
                        elements.push(comments);
                    }

                    element = unit!();
                    comments = unit!();
                    separator = node_to_layout_expr(
                        child,
                        source_code,
                        config,
                        choice_nest_level + choice_nest,
                    );

                    last_comment = true;
                } else {
                    if !element_apposable {
                        elements.push(element);
                        element = unit!();
                    }

                    separator = node_to_layout_expr(
                        child,
                        source_code,
                        config,
                        choice_nest_level + choice_nest,
                    );
                }
            }

            kind_id => {
                if after_open {
                    after_open = false;

                    if kind_id == KindId::MULTIPLE_NEWLINES {
                        continue;
                    }
                }

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

                if !comments.is_unit() {
                    if separator.is_unit() {
                        elements.push(element);
                        elements.push(comments);
                    } else {
                        elements.push(apposition!(element, separator, text!(" "), comments));
                    }
                } else {
                    elements.push(apposition!(element, separator))
                }

                comments = unit!();
                separator = unit!();
                element = node_to_layout_expr(
                    child,
                    source_code,
                    config,
                    choice_nest_level + choice_nest,
                );

                element_apposable = is_apposable(kind_id);
                has_non_apposable |= !element_apposable;

                if kind_id == KindId::LINE_COMMENT {
                    last_comment = true;
                } else {
                    last_comment = false;
                }
            }
        }
    }

    if !comments.is_unit() {
        if !element.is_unit() {
            if element_apposable {
                elements.push(apposition!(element, text!(" "), comments));
            } else {
                elements.push(element);
            }
        } else {
            elements.push(comments);
        }

        last_comment = true;
    } else {
        if !element.is_empty() {
            elements.push(element);
        }
    }

    let mut num_elements = 0;

    let mut stacked_elements = unit!();
    let mut apposed_elements = unit!();

    for element in elements {
        if !element.is_unit() {
            num_elements += 1;
        }

        stacked_elements = stack!(stacked_elements, element.clone());

        if apposed_elements == unit!() {
            apposed_elements = element;
        } else {
            apposed_elements = apposition!(apposed_elements, text!(" "), element);
        }
    }

    match kind_id {
        KindId::EXPORT_ATTRIBUTE_BLOCK => {
            if has_non_apposable || last_comment {
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

        KindId::FUNCTION_CLAUSE_BLOCK => {
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
                        apposition!(open.clone(), text!(" ")),
                        stack!(open, text!("    "))
                    ),
                    stack!(
                        stacked_elements,
                        if last_comment { text!("") } else { unit!() }
                    )
                )
            }
        }

        KindId::FUNCTION_CALL_BLOCK => {
            let body = if has_non_apposable || choice_nest_level > config.max_choice_nest_level {
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

        KindId::LIST => {
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

fn kind_id(node: Node<'_>) -> KindId {
    unsafe { std::mem::transmute(node.kind_id()) }
}

fn is_apposable(kind_id: KindId) -> bool {
    match kind_id {
        KindId::MULTIPLE_NEWLINES | KindId::COMMENT | KindId::LINE_COMMENT => false,
        _ => true,
    }
}

#[cfg(test)]
mod parse_test {
    use super::{parse, Config};
    use indoc::indoc;

    macro_rules! assert_parse {
        ($desc:literal, $source_code:expr $(,)?) => {{
            let source_code = $source_code;
            let config = &Config {
                max_choice_nest_level: 100,
            };
            pretty_assertions::assert_eq!(
                parse(source_code, config).format(0, false).0 + "\n",
                source_code,
                $desc
            )
        }};

        ($desc:literal, $source_code:expr, $expected:expr $(,)?) => {{
            let config = &Config {
                max_choice_nest_level: 100,
            };
            pretty_assertions::assert_eq!(
                parse($source_code, config).format(0, false).0 + "\n",
                $expected,
                $desc
            )
        }};
    }

    #[test]
    fn node_to_apposed_layout_expr() {
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
    #[ignore]
    fn node_to_stacked_layout_expr() {
        todo!()
    }

    #[test]
    fn node_to_stacked_or_apposed_layout_expr() {
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
    fn trailing_separator_node_to_layout_expr() {
        /*
        assert_parse!(
            "remove empty lines and trailing separator",
            indoc! {r#"
                main() ->
                    ok

                    % comment

                    ,.
            "#},
            indoc! {r#"
                main() ->
                    ok

                    % comment
                    .
            "#},
        );

        assert_parse!(
            "comments indentation",
            indoc! {r#"
                main() -> ok % comment 1
                             % comment 2
                          , % comment 3
                            % comment 4
                          .
            "#},
            indoc! {r#"
                main() -> ok % comment 1
                             % comment 2
                           % comment 3
                           % comment 4
                           .
            "#},
        );
        */

        assert_parse!(
            "line comments indentation",
            indoc! {r#"
                main() ->
                    ok
                    %% line comment 1
                    %% line comment 2
                    ,
                    %% line comment 3
                    %% line comment 4
                    .
            "#},
            indoc! {r#"
                main() ->
                    ok
                    %% line comment 1
                    %% line comment 2
                     %% line comment 3
                     %% line comment 4
                     .
            "#},
        );
    }

    #[test]
    #[ignore]
    fn binary_expression_node_to_layout_expr_add() {
        todo!()
    }

    #[test]
    #[ignore]
    fn binary_expression_node_to_layout_expr_equal() {
        todo!()
    }

    #[test]
    fn export_style_block_expression_node_to_layout_expr() {
        assert_parse!(
            "remove some empty lines",
            indoc! {r#"
                -export([

                         %% comment 1

                         main/0

                         %% comment 2

                        ]).
            "#},
            indoc! {r#"
                -export([
                         %% comment 1

                         main/0

                         %% comment 2
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
                         main/0 % comment 2
                         , % comment 3
                        ]).
            "#},
            indoc! {r#"
                -export([
                         % comment 1
                         main/0 % comment 2
                         % comment 3
                        ]).
            "#},
        );
    }

    #[test]
    fn function_style_block_expression_node_to_layout_expr() {
        assert_parse!(
            "remove some empty lines",
            indoc! {r#"
                main() ->

                    ok.
            "#},
            indoc! {r#"
                main() -> ok.
            "#},
        );

        assert_parse!(
            "comment indentation",
            indoc! {r#"
                main() ->
                    % comment 1
                    % comment 2
                    ok.
            "#},
        );

        assert_parse!(
            "line comment indentation",
            indoc! {r#"
                main() ->
                    %% line comment 1
                    %% line comment 2
                    ok.
            "#},
        );

        assert_parse!(
            "remove trailing separator",
            indoc! {r#"
                main() -> ok,.
            "#},
            indoc! {r#"
                main() -> ok.
            "#},
        );
    }

    #[test]
    fn function_call_style_block_expression_node_to_layout_expr() {
        assert_parse!(
            "remove some empty lines",
            indoc! {r#"
                main() -> mod:function(

                            "foo"

                            ).
            "#},
            indoc! {r#"
                main() -> mod:function(
                            "foo").
            "#},
        );

        assert_parse!(
            "comments indentation",
            indoc! {r#"
                main() -> mod:function(
                            % comment 1
                            % comment 2
                            "foo" % comment 3
                                  % comment 4
                            ).
            "#},
        );

        assert_parse!(
            "line comments indentation",
            indoc! {r#"
                main() -> mod:function(
                            %% line comment 1
                            %% line comment 2
                            "foo"
                            %% line comment 3
                            %% line comment 4
                            ).
            "#},
        );

        assert_parse!(
            "remove trailing separator",
            indoc! {r#"
                main() -> mod:function(
                            "foo",).
            "#},
            indoc! {r#"
                main() -> mod:function(
                            "foo").
            "#},
        );
    }
}

#[cfg(test)]
mod format_test {
    use super::layout_fun::Config as LayoutFunConfig;
    use super::{format, Config};
    use indoc::indoc;

    macro_rules! assert_format {
        ($desc:literal, $layout_fun_config:expr, $source_code:expr $(,)?) => {{
            let source_code = $source_code;
            let config = &Config{
                max_choice_nest_level: 100
            };
            pretty_assertions::assert_eq!(
                format(source_code, config, &$layout_fun_config) + "\n",
                source_code,
                $desc
            )
        }};

        ($desc:literal, $layout_fun_config:expr, $source_code:expr, $expected:expr $(,)?) => {{
            let config = &Config{
                max_choice_nest_level: 100;
            }
            pretty_assertions::assert_eq!(
                format($source_code, config, &$layout_fun_config) + "\n",
                $expected,
                $desc
            )
        }};
    }

    #[test]
    fn node_to_stacked_or_apposed_layout_expr() {
        assert_format!(
            "apposed",
            LayoutFunConfig {
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
            LayoutFunConfig {
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
            LayoutFunConfig {
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
            "stacked due to line comments",
            LayoutFunConfig {
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
            LayoutFunConfig {
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
    #[ignore]
    fn binary_expression_add() {
        todo!()
    }

    #[test]
    #[ignore]
    fn binary_expression_equal() {
        todo!()
    }

    #[test]
    fn export_style_block_expression_node_to_layout_expr() {
        assert_format!(
            "apposed",
            LayoutFunConfig {
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
            "apposed with a multi line element",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([foo/ % comment
                              1]).
            "#},
        );

        assert_format!(
            "stacked due to right margin",
            LayoutFunConfig {
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

        assert_format!(
            "stacked due to a before body comment",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([
                         % comment
                         foo/1
                        ]).
            "#},
        );

        assert_format!(
            "stacked due to an after body comment",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([
                         foo/1 % comment
                        ]).
            "#},
        );

        assert_format!(
            "stacked due to a before body line comment",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([
                         %% line comment
                         foo/1
                        ]).
            "#},
        );

        assert_format!(
            "stacked due to an after body line comment",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                -export([
                         foo/1
                         %% line comment
                        ]).
            "#},
        );
    }

    #[test]
    fn function_style_block_expression_node_to_layout_expr() {
        assert_format!(
            "apposed",
            LayoutFunConfig {
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
            LayoutFunConfig {
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
            "stacked due to a before body comment",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                f() ->
                    % comment
                    ok.
            "#}
        );

        assert_format!(
            "stacked due to a before body line comment",
            LayoutFunConfig {
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
            LayoutFunConfig {
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

    #[test]
    fn function_call_style_block_expression_node_to_layout_expr() {
        assert_format!(
            "apposed",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                main() ->
                    %% line comment
                    mod:function("foo").
            "#},
        );

        assert_format!(
            "apposed with multi line body",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                main() ->
                    %% line comment
                    mod:function(mod: % comment
                                      function).
            "#},
        );

        assert_format!(
            "apposed with last comment",
            LayoutFunConfig {
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
        );

        assert_format!(
            "apposed with last line comment",
            LayoutFunConfig {
                right_margin: 80,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                main() ->
                    %% line comment
                    mod:function("foo"
                                 %% line comment
                                 ).
            "#},
        );

        assert_format!(
            "stacked due to right margin",
            LayoutFunConfig {
                right_margin: 23,
                newline_cost: 1,
                beyond_right_margin_cost: 10000,
                height_cost: 100,
            },
            indoc! {r#"
                main() ->
                    %% line comment
                    mod:function(
                      "foo").
            "#},
        );
    }
}
