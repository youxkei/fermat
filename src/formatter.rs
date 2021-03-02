use std::rc::Rc;

use crate::layout_expr::{
    apposition, apposition_sep, choice, multi_line_cost, stack, text, unit, LayoutExpr,
};
use crate::layout_fun::{Config, LayoutFun};
use crate::node::{kind_id, KindId, Node, Tree};

pub fn format(tree: Tree, source_code: &str, config: &Config) -> String {
    let layout_expr = node_to_layout_expr(tree.root_node(), source_code);
    let layout_fun = LayoutFun::from_layout_expr(&layout_expr, config);

    let calculated = layout_fun.at(0);

    calculated.layout_expr.format(0, false).0 + "\n"
}

fn node_to_layout_expr<'a>(node: Node<'_>, source_code: &'a str) -> Rc<LayoutExpr<'a>> {
    let kind_id = kind_id(node);

    match kind_id {
        KindId::FORM
        | KindId::MODULE_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE
        | KindId::EXPORT_ATTRIBUTE_MFA
        | KindId::RECORD_ATTRIBUTE_OPEN
        | KindId::TYPE_ATTRIBUTE_BEGIN
        | KindId::TYPE_ATTRIBUTE_NAME
        | KindId::SPEC_ATTRIBUTE
        | KindId::TYPE_SPEC
        | KindId::TYPE_SPEC_WITH_PAREN
        | KindId::SPEC_FUN_NAME
        | KindId::TYPE_GUARD
        | KindId::FUN_TYPE_OPEN
        | KindId::TYPE
        | KindId::BINARY_BASE_TYPE
        | KindId::BINARY_UNIT_TYPE
        | KindId::OTHER_ATTRIBUTE_OPEN
        | KindId::NO_PAREN_ATTRIBUTE
        | KindId::FUNCTION_CLAUSE_OPEN
        | KindId::CLAUSE_GUARD
        | KindId::UNARY_EXPR
        | KindId::MAP_EXPR_OPEN
        | KindId::FUNCTION_CALL_OPEN
        | KindId::RECORD_INDEX_EXPR
        | KindId::RECORD_EXPR_OPEN
        | KindId::REMOTE_EXPR
        | KindId::LIST_CLOSE
        | KindId::LIST_TAIL
        | KindId::BINARY_ELEMENT
        | KindId::LIST_COMPREHENSION
        | KindId::LIST_COMPREHENSION_CLAUSE
        | KindId::BINARY_COMPREHENSION
        | KindId::BINARY_COMPREHENSION_CLAUSE
        | KindId::COMPREHENSION_CLAUSE_EXPR
        | KindId::PAREN_EXPR
        | KindId::IF_EXPR_CLAUSE_OPEN
        | KindId::CASE_EXPR_BEGIN
        | KindId::CASE_EXPR_BEGIN_TAIL
        | KindId::RECEIVE_EXPR_AFTER_CLAUSE_OPEN
        | KindId::MATCH_CLAUSE_OPEN
        | KindId::FUN_REF_EXPR
        | KindId::FUN_REF_EXPR_TAIL
        | KindId::FUN_CLAUSE_OPEN
        | KindId::FUN_CLAUSE_WITH_HEAD_OPEN
        | KindId::TRY_EXPR_CATCH_CLAUSE_OPEN
        | KindId::PAT_UNARY_EXPR
        | KindId::PAT_MAP_EXPR_OPEN
        | KindId::PAT_RECORD_INDEX_EXPR
        | KindId::PAT_RECORD_EXPR_OPEN
        | KindId::PAT_LIST_CLOSE
        | KindId::PAT_LIST_TAIL
        | KindId::PAT_BINARY_ELEMENT
        | KindId::PAT_PAREN_EXPR => elements_node_to_apposed_layout_expr(node, source_code),

        KindId::SOURCE_FILE
        | KindId::EXPORT_ATTRIBUTE_MFAS
        | KindId::TYPE_ATTRIBUTE_PARAMETERS
        | KindId::RECORD_ATTRIBUTE
        | KindId::RECORD_FIELDS
        | KindId::TYPE_SIGS
        | KindId::TYPE_GUARDS
        | KindId::FUN_TYPE
        | KindId::TOP_TYPES
        | KindId::MAP_FIELD_TYPES
        | KindId::RECORD_FIELD_TYPES
        | KindId::BINARY_EXPR_TYPE
        | KindId::OTHER_ATTRIBUTE
        | KindId::FUNCTION_CLAUSES
        | KindId::FUNCTION_CLAUSE
        | KindId::PAT_PARAMETERS
        | KindId::MAP_EXPR
        | KindId::FUNCTION_CALL
        | KindId::RECORD_EXPR
        | KindId::GUARD
        | KindId::EXPRS
        | KindId::LIST
        | KindId::BINARY
        | KindId::LIST_COMPREHENSION_CLAUSES
        | KindId::BINARY_COMPREHENSION_CLAUSES
        | KindId::TUPLE
        | KindId::BEGIN_END_EXPR
        | KindId::IF_EXPR
        | KindId::IF_EXPR_CLAUSES
        | KindId::IF_EXPR_CLAUSE
        | KindId::CASE_EXPR
        | KindId::CASE_EXPR_CLAUSES
        | KindId::RECEIVE_EXPR
        | KindId::RECEIVE_EXPR_CLAUSES
        | KindId::RECEIVE_EXPR_AFTER
        | KindId::RECEIVE_EXPR_AFTER_CLAUSE
        | KindId::MATCH_CLAUSE
        | KindId::FUN_EXPR
        | KindId::FUN_CLAUSE
        | KindId::FUN_EXPR_WITH_HEAD
        | KindId::FUN_CLAUSE_WITH_HEAD
        | KindId::TRY_EXPR
        | KindId::TRY_EXPR_TRY
        | KindId::TRY_EXPR_OF
        | KindId::TRY_EXPR_CATCH
        | KindId::TRY_EXPR_CATCH_CLAUSE
        | KindId::TRY_EXPR_AFTER
        | KindId::PAT_MAP_EXPR
        | KindId::PAT_RECORD_EXPR
        | KindId::PAT_LIST
        | KindId::PAT_BINARY
        | KindId::PAT_TUPLE
        | KindId::STRINGS => elements_node_to_layout_expr(node, source_code),

        KindId::RECORD_FIELD
        | KindId::RECORD_FIELD_NAME_WITH_DEFAULT_VALUE
        | KindId::TYPE_ATTRIBUTE
        | KindId::TYPE_SIG
        | KindId::BIND_TYPE_GUARD
        | KindId::BINARY_TOP_TYPE
        | KindId::BINARY_TYPE
        | KindId::MAP_FIELD_TYPE
        | KindId::RECORD_FIELD_TYPE
        | KindId::BINARY_EXPR
        | KindId::MAP_EXPR_FIELD
        | KindId::RECORD_EXPR_FIELD
        | KindId::LIST_COMPREHENSION_CONTENT
        | KindId::BINARY_COMPREHENSION_CONTENT
        | KindId::PAT_BINARY_EXPR
        | KindId::PAT_MAP_EXPR_FIELD
        | KindId::PAT_RECORD_EXPR_FIELD => binary_expression_node_to_layout_expr(node, source_code),

        KindId::HYPHEN_GREATER
        | KindId::PAREN_OPEN
        | KindId::PAREN_CLOSE
        | KindId::BRACE_OPEN
        | KindId::BRACE_CLOSE
        | KindId::BRACKET_OPEN
        | KindId::BRACKET_CLOSE
        | KindId::LESS_LESS_OPEN
        | KindId::GREATER_GREATER_CLOSE
        | KindId::BEGIN_OPEN
        | KindId::END_CLOSE
        | KindId::FUN_OPEN
        | KindId::MODULE
        | KindId::EXPORT
        | KindId::EXPORT_TYPE
        | KindId::RECORD
        | KindId::TYPE_OR_OPAQUE
        | KindId::SPEC
        | KindId::CALLBACK
        | KindId::WHEN
        | KindId::CATCH
        | KindId::IF
        | KindId::CASE
        | KindId::OF
        | KindId::RECEIVE
        | KindId::AFTER
        | KindId::FUN
        | KindId::TRY
        | KindId::END
        | KindId::COMMA
        | KindId::PERIOD
        | KindId::HYPHEN
        | KindId::SLASH
        | KindId::SEMICOLON
        | KindId::COLON
        | KindId::QUOTEDBL
        | KindId::BAR
        | KindId::NUMBERSIGN
        | KindId::LESS_HYPHEN
        | KindId::LESS_EQUAL
        | KindId::PERIOD_PERIOD_PERIOD
        | KindId::ASTERISK
        | KindId::UNDERSCORE
        | KindId::TYPE_BIND_OP
        | KindId::MAP_TYPE_OP
        | KindId::UNION_OP
        | KindId::RANGE_OP
        | KindId::ORELSE_OP
        | KindId::ANDALSO_OP
        | KindId::EQUAL_OP
        | KindId::EXCLAM_OP
        | KindId::COMP_OP
        | KindId::LIST_OP
        | KindId::ADD_OP
        | KindId::MULT_OP
        | KindId::PREFIX_NOSPACE_OP
        | KindId::PREFIX_SPACE_OP
        | KindId::MAP_OP
        | KindId::COMPREHENSION_OP
        | KindId::WHEN_OP
        | KindId::LIST_TYPE_TAIL
        | KindId::VARIABLE
        | KindId::ATOM
        | KindId::MACRO
        | KindId::CHAR
        | KindId::INTEGER
        | KindId::FLOAT
        | KindId::STRING => text!(&source_code[node.start_byte()..node.end_byte()]),

        KindId::COMMENT | KindId::LINE_COMMENT => {
            text!(&source_code[node.start_byte()..node.end_byte()], 0)
        }

        KindId::SPACES | KindId::NEWLINE | KindId::MULTIPLE_NEWLINES => text!(""),

        KindId::ERROR => panic!("syntax error {:?}", node),
    }
}

fn elements_node_to_apposed_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
) -> Rc<LayoutExpr<'a>> {
    let mut result = unit!();
    let mut comments = unit!();

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match self::kind_id(child) {
            KindId::SPACES | KindId::NEWLINE | KindId::MULTIPLE_NEWLINES => {}

            KindId::COMMENT | KindId::LINE_COMMENT => {
                comments = stack!(comments, node_to_layout_expr(child, source_code))
            }

            KindId::HYPHEN_GREATER
            | KindId::LESS_HYPHEN
            | KindId::LESS_EQUAL
            | KindId::WHEN
            | KindId::GUARD
            | KindId::BAR
            | KindId::OF
            | KindId::TYPE_ATTRIBUTE_NAME
            | KindId::TYPE_SPEC
            | KindId::TYPE_GUARDS
            | KindId::LIST_TAIL
            | KindId::LIST_TYPE_TAIL
            | KindId::COMPREHENSION_CLAUSE_EXPR
            | KindId::PAT_LIST_TAIL
            | KindId::CASE_EXPR_BEGIN_TAIL
            | KindId::FUN_REF_EXPR_TAIL => {
                result = apposition!(
                    result,
                    text!(" "),
                    stack!(comments, node_to_layout_expr(child, source_code))
                );

                comments = unit!();
            }

            KindId::CATCH | KindId::PREFIX_SPACE_OP => {
                result = apposition!(node_to_layout_expr(child, source_code), text!(" "),)
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

fn elements_node_to_layout_expr<'a>(node: Node<'_>, source_code: &'a str) -> Rc<LayoutExpr<'a>> {
    let kind_id = kind_id(node);

    let introduce_multi_line_cost = match kind_id {
        _ => true,
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
            KindId::SPACES | KindId::NEWLINE => {}

            KindId::COMMENT => {
                after_open = false;

                if extras.is_unit() && last_extra.is_unit() {
                    comments = stack!(comments, node_to_layout_expr(child, source_code,))
                } else {
                    extras = stack!(extras, last_extra);
                    last_extra = node_to_layout_expr(child, source_code)
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
                last_extra = node_to_layout_expr(child, source_code);
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

                separator = node_to_layout_expr(child, source_code)
            }

            kind_id => {
                after_open = false;

                if kind_id.is_open() {
                    open = node_to_layout_expr(child, source_code);
                    after_open = true;

                    continue;
                }

                if kind_id.is_close() {
                    close = node_to_layout_expr(child, source_code);

                    continue;
                }

                if last_extra.is_empty() {
                    has_extra = true;
                }

                element = apposition!(element, separator);

                elements.push(apposition_sep!(text!(" "), element, comments));
                elements.push(stack!(extras, last_extra));

                element = node_to_layout_expr(child, source_code);

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

        if introduce_multi_line_cost {
            apposed_elements =
                apposition_sep!(text!(" "), apposed_elements, multi_line_cost!(element));
        } else {
            apposed_elements = apposition_sep!(text!(" "), apposed_elements, element);
        }
    }

    match kind_id {
        KindId::SOURCE_FILE => stacked_elements,

        KindId::TYPE_SIGS
        | KindId::TYPE_GUARDS
        | KindId::MAP_FIELD_TYPES
        | KindId::RECORD_FIELD_TYPES
        | KindId::FUNCTION_CLAUSES
        | KindId::IF_EXPR
        | KindId::CASE_EXPR
        | KindId::RECEIVE_EXPR
        | KindId::TRY_EXPR
        | KindId::STRINGS => {
            if last_comment {
                stack!(stacked_elements, text!(""))
            } else {
                stacked_elements
            }
        }

        KindId::IF_EXPR_CLAUSES
        | KindId::CASE_EXPR_CLAUSES
        | KindId::RECEIVE_EXPR_CLAUSES
        | KindId::RECEIVE_EXPR_AFTER
        | KindId::TRY_EXPR_TRY
        | KindId::TRY_EXPR_OF
        | KindId::TRY_EXPR_CATCH
        | KindId::TRY_EXPR_AFTER => {
            apposition!(text!("    "), stacked_elements)
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
        }

        KindId::FUN_TYPE
        | KindId::IF_EXPR_CLAUSE
        | KindId::RECEIVE_EXPR_AFTER_CLAUSE
        | KindId::MATCH_CLAUSE
        | KindId::FUN_CLAUSE
        | KindId::FUN_CLAUSE_WITH_HEAD
        | KindId::TRY_EXPR_CATCH_CLAUSE => {
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

        KindId::MAP_EXPR
        | KindId::RECORD_EXPR
        | KindId::PAT_MAP_EXPR
        | KindId::PAT_RECORD_EXPR
        | KindId::FUNCTION_CALL => {
            let body = if has_extra || num_elements == 1 {
                stacked_elements
            } else {
                choice!(stacked_elements, apposed_elements)
            };

            let tail = if last_comment {
                stack!(body, close)
            } else {
                apposition!(body, close)
            };

            apposition!(choice!(stack!(open.clone(), text!("  ")), open), tail)
        }

        KindId::BEGIN_END_EXPR => {
            if num_elements > 1 || last_comment {
                stack!(open, apposition!(text!("    "), stacked_elements,), close)
            } else {
                choice!(
                    stack!(
                        open.clone(),
                        apposition!(text!("    "), stacked_elements,),
                        close.clone()
                    ),
                    apposition!(open, text!(" "), apposed_elements, text!(" "), close)
                )
            }
        }

        KindId::FUN_EXPR => {
            stack!(apposition!(open, stacked_elements), close)
        }

        KindId::FUN_EXPR_WITH_HEAD => {
            stack!(apposition!(open, text!(" "), stacked_elements,), close)
        }

        KindId::TOP_TYPES | KindId::LIST | KindId::TUPLE => {
            if has_extra || num_elements > 5 || num_elements <= 1 {
                apposition!(
                    open,
                    stack!(
                        stacked_elements,
                        if last_comment { text!("") } else { unit!() }
                    ),
                    close
                )
            } else {
                apposition!(
                    open,
                    stack!(
                        choice!(stacked_elements, apposed_elements),
                        if last_comment { text!("") } else { unit!() }
                    ),
                    close
                )
            }
        }

        KindId::RECORD_ATTRIBUTE
        | KindId::RECORD_FIELDS
        | KindId::TYPE_ATTRIBUTE_PARAMETERS
        | KindId::BINARY_EXPR_TYPE
        | KindId::OTHER_ATTRIBUTE
        | KindId::PAT_PARAMETERS
        | KindId::GUARD
        | KindId::EXPRS
        | KindId::BINARY
        | KindId::LIST_COMPREHENSION_CLAUSES
        | KindId::BINARY_COMPREHENSION_CLAUSES
        | KindId::PAT_LIST
        | KindId::PAT_BINARY
        | KindId::PAT_TUPLE => {
            if has_extra || num_elements <= 1 {
                apposition!(
                    open,
                    stack!(
                        stacked_elements,
                        if last_comment { text!("") } else { unit!() }
                    ),
                    close
                )
            } else {
                apposition!(
                    open,
                    stack!(
                        choice!(stacked_elements, apposed_elements),
                        if last_comment { text!("") } else { unit!() }
                    ),
                    close
                )
            }
        }

        _ => panic!("{:?} is not covered", kind_id),
    }
}

fn binary_expression_node_to_layout_expr<'a>(
    node: Node<'_>,
    source_code: &'a str,
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
                KindId::SPACES | KindId::NEWLINE | KindId::MULTIPLE_NEWLINES => {}

                KindId::COMMENT | KindId::LINE_COMMENT => {
                    comments_between_lhs_and_op = stack!(
                        comments_between_lhs_and_op,
                        node_to_layout_expr(child, source_code)
                    )
                }

                kind_id => {
                    if kind_id.is_op() {
                        op = node_to_layout_expr(child, source_code);
                        op_kind_id = kind_id;
                        phase = Phase::Rhs;
                    } else {
                        lhs = node_to_layout_expr(child, source_code);
                    }
                }
            },

            Phase::Rhs => match self::kind_id(child) {
                KindId::SPACES | KindId::NEWLINE | KindId::MULTIPLE_NEWLINES => {}

                KindId::COMMENT | KindId::LINE_COMMENT => {
                    comments_between_op_and_rhs = stack!(
                        comments_between_op_and_rhs,
                        node_to_layout_expr(child, source_code)
                    )
                }

                _ => rhs = node_to_layout_expr(child, source_code),
            },
        }
    }

    match op_kind_id {
        KindId::UNION_OP
        | KindId::RANGE_OP
        | KindId::EXCLAM_OP
        | KindId::ORELSE_OP
        | KindId::ANDALSO_OP
        | KindId::COMP_OP
        | KindId::LIST_OP
        | KindId::ADD_OP
        | KindId::MULT_OP
        | KindId::COMPREHENSION_OP
        | KindId::WHEN_OP => {
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

        KindId::TYPE_BIND_OP | KindId::MAP_TYPE_OP | KindId::EQUAL_OP | KindId::MAP_OP => {
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

        KindId::ERROR => lhs,

        _ => panic!("{:?} is not covered", op_kind_id),
    }
}

#[cfg(test)]
mod node_to_layout_expr_test {
    use crate::formatter::node_to_layout_expr;
    use crate::node::parse;

    macro assert_parse {
        ($source_code:expr $(,)?) => {{
            let source_code = $source_code;

            pretty_assertions::assert_eq!(
                node_to_layout_expr(parse(source_code).root_node(), source_code).format(0, false).0 + "\n",
                source_code
            )
        }},

        ($source_code:expr, $expected:expr $(,)?) => {{
            let source_code = $source_code;

            pretty_assertions::assert_eq!(
                node_to_layout_expr(parse(source_code).root_node(), source_code).format(0, false).0 + "\n",
                $expected
            )
        }},
    }

    mod elements_node_to_apposed_layout_expr {
        use crate::formatter::node_to_layout_expr_test::assert_parse;
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
        use crate::formatter::node_to_layout_expr_test::assert_parse;
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
            use crate::formatter::node_to_layout_expr_test::assert_parse;
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
            use crate::formatter::node_to_layout_expr_test::assert_parse;
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
    use crate::formatter::format;
    use crate::node::parse;

    macro assert_format {
        ($config:expr, $source_code:expr $(,)?) => {{
            let source_code = $source_code;

            pretty_assertions::assert_eq!(
                format(parse(source_code), source_code, &$config),
                source_code
            )
        }},

        ($config:expr, $source_code:expr, $expected:expr $(,)?) => {{
            let source_code = $source_code;

            pretty_assertions::assert_eq!(
                format(parse(source_code), source_code, &$config),
                $expected
            )
        }},
    }

    mod elements_node_to_layout_expr {
        mod export_attribute {
            use crate::formatter::format_test::assert_format;
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
            fn stacked_due_to_right_margin() {
                assert_format!(
                    Config {
                        right_margin: 10,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
            fn stacked_due_to_multi_line_element() {
                assert_format!(
                    Config {
                        right_margin: 80,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                        max_choice_nest_level: 100,
                    },
                    indoc! {r#"
                        -export([
                                 foo/1,
                                 bar/2,
                                 foobar/ % comment
                                         3,
                                 baz/4
                                ]).
                    "#},
                );
            }
        }

        mod function_call {
            use crate::formatter::format_test::assert_format;
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
                        right_margin: 22,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                        max_choice_nest_level: 100,
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
                        right_margin: 22,
                        newline_cost: 1,
                        beyond_right_margin_cost: 10000,
                        height_cost: 100,
                        max_choice_nest_level: 100,
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
            use crate::formatter::format_test::assert_format;
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
            use crate::formatter::format_test::assert_format;
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
                        max_choice_nest_level: 100,
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
                        max_choice_nest_level: 100,
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
