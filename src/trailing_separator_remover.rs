use std::fmt::Write;

use crate::node::{kind_id, KindId, Node, Tree};

pub fn remove_trailing_separators(tree: Tree, source_code: &str) -> String {
    node_to_removed_code(tree.root_node(), source_code)
}

fn node_to_removed_code(node: Node<'_>, source_code: &str) -> String {
    match kind_id(node) {
        KindId::SOURCE_FILE
        | KindId::FORM
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
        | KindId::PAT_PAREN_EXPR
        | KindId::RECORD_FIELD
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
        | KindId::PAT_RECORD_EXPR_FIELD
        | KindId::STRINGS => {
            let mut result = String::new();

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                write!(&mut result, "{}", node_to_removed_code(child, source_code)).unwrap();
            }

            result
        }

        KindId::EXPORT_ATTRIBUTE_MFAS
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
        | KindId::PAT_TUPLE => {
            let mut result = String::new();
            let mut element = String::new();
            let mut separator = String::new();
            let mut has_non_extra = false;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                let kind_id = kind_id(child);

                match kind_id {
                    KindId::COMMA | KindId::SEMICOLON => {
                        write!(&mut result, "{}{}", separator, element).unwrap();

                        separator = node_to_removed_code(child, source_code);
                        element = String::new();
                        has_non_extra = false;
                    }

                    _ => {
                        write!(&mut element, "{}", node_to_removed_code(child, source_code))
                            .unwrap();

                        if kind_id.is_close() {
                            continue;
                        }

                        match kind_id {
                            KindId::COMMENT
                            | KindId::LINE_COMMENT
                            | KindId::NEWLINE
                            | KindId::MULTIPLE_NEWLINES
                            | KindId::SPACES => {}

                            _ => has_non_extra = true,
                        }
                    }
                }
            }

            if has_non_extra {
                write!(&mut result, "{}", separator).unwrap();
            }

            write!(&mut result, "{}", element).unwrap();

            result
        }

        KindId::SPACES
        | KindId::COMMENT
        | KindId::LINE_COMMENT
        | KindId::NEWLINE
        | KindId::MULTIPLE_NEWLINES
        | KindId::HYPHEN_GREATER
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
        | KindId::STRING => source_code[node.start_byte()..node.end_byte()].to_string(),

        KindId::ERROR => panic!("syntax error {:?}", node),
    }
}
