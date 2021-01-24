#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 82
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 67
#define ALIAS_COUNT 1
#define TOKEN_COUNT 34
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 5

enum {
  sym_atom = 1,
  anon_sym_DOT = 2,
  anon_sym_DASH = 3,
  anon_sym_module = 4,
  anon_sym_LPAREN = 5,
  anon_sym_RPAREN = 6,
  anon_sym_export = 7,
  anon_sym_LBRACK = 8,
  anon_sym_COMMA = 9,
  anon_sym_RBRACK = 10,
  anon_sym_SLASH = 11,
  anon_sym_SEMI = 12,
  anon_sym_DASH_GT = 13,
  anon_sym_when = 14,
  anon_sym_COLON = 15,
  anon_sym_EQ = 16,
  anon_sym_PLUS = 17,
  anon_sym_bor = 18,
  anon_sym_bxor = 19,
  anon_sym_bsl = 20,
  anon_sym_bsr = 21,
  anon_sym_or = 22,
  anon_sym_xor = 23,
  sym_variable = 24,
  sym_integer = 25,
  anon_sym_DQUOTE = 26,
  aux_sym_string_token1 = 27,
  sym__escape_sequence = 28,
  sym_comment = 29,
  sym_line_comment = 30,
  sym__newline = 31,
  sym_multiple_newlines = 32,
  sym__spaces = 33,
  sym_source_file = 34,
  sym_form = 35,
  sym__attribute = 36,
  sym_module_attribute = 37,
  sym_export_attribute = 38,
  sym_export_attribute_block = 39,
  sym_export_attribute_mfas = 40,
  sym_export_attribute_mfa = 41,
  sym__function_or_macro = 42,
  sym_function_clauses_trailing_semicolon = 43,
  sym_function_clauses = 44,
  sym_function_clause_block = 45,
  sym_function_clause_open = 46,
  sym_pat_argument_list = 47,
  sym_clause_guard = 48,
  sym_function_clause_exprs_trailing_comma = 49,
  sym_exprs = 50,
  sym_expr = 51,
  sym_function_call_block = 52,
  sym_function_call_open = 53,
  sym_remote_expr = 54,
  sym_expr_max = 55,
  sym_equal_op = 56,
  sym_add_op = 57,
  sym__atomic = 58,
  sym_strings = 59,
  sym_string = 60,
  aux_sym_source_file_repeat1 = 61,
  aux_sym_export_attribute_mfas_repeat1 = 62,
  aux_sym_function_clauses_repeat1 = 63,
  aux_sym_exprs_repeat1 = 64,
  aux_sym_strings_repeat1 = 65,
  aux_sym_string_repeat1 = 66,
  anon_alias_sym_function_clause_exprs = 67,
};

static const char *ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_atom] = "atom",
  [anon_sym_DOT] = ".",
  [anon_sym_DASH] = "-",
  [anon_sym_module] = "module",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_export] = "export",
  [anon_sym_LBRACK] = "[",
  [anon_sym_COMMA] = ",",
  [anon_sym_RBRACK] = "]",
  [anon_sym_SLASH] = "/",
  [anon_sym_SEMI] = ";",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_when] = "when",
  [anon_sym_COLON] = ":",
  [anon_sym_EQ] = "=",
  [anon_sym_PLUS] = "+",
  [anon_sym_bor] = "bor",
  [anon_sym_bxor] = "bxor",
  [anon_sym_bsl] = "bsl",
  [anon_sym_bsr] = "bsr",
  [anon_sym_or] = "or",
  [anon_sym_xor] = "xor",
  [sym_variable] = "variable",
  [sym_integer] = "integer",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_token1",
  [sym__escape_sequence] = "_escape_sequence",
  [sym_comment] = "comment",
  [sym_line_comment] = "line_comment",
  [sym__newline] = "_newline",
  [sym_multiple_newlines] = "multiple_newlines",
  [sym__spaces] = "_spaces",
  [sym_source_file] = "source_file",
  [sym_form] = "form",
  [sym__attribute] = "_attribute",
  [sym_module_attribute] = "module_attribute",
  [sym_export_attribute] = "export_attribute",
  [sym_export_attribute_block] = "export_attribute_block",
  [sym_export_attribute_mfas] = "export_attribute_mfas",
  [sym_export_attribute_mfa] = "export_attribute_mfa",
  [sym__function_or_macro] = "_function_or_macro",
  [sym_function_clauses_trailing_semicolon] = "function_clauses_trailing_semicolon",
  [sym_function_clauses] = "function_clauses",
  [sym_function_clause_block] = "function_clause_block",
  [sym_function_clause_open] = "function_clause_open",
  [sym_pat_argument_list] = "pat_argument_list",
  [sym_clause_guard] = "clause_guard",
  [sym_function_clause_exprs_trailing_comma] = "function_clause_exprs_trailing_comma",
  [sym_exprs] = "exprs",
  [sym_expr] = "expr",
  [sym_function_call_block] = "function_call_block",
  [sym_function_call_open] = "function_call_open",
  [sym_remote_expr] = "remote_expr",
  [sym_expr_max] = "expr_max",
  [sym_equal_op] = "equal_op",
  [sym_add_op] = "add_op",
  [sym__atomic] = "_atomic",
  [sym_strings] = "strings",
  [sym_string] = "string",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_export_attribute_mfas_repeat1] = "export_attribute_mfas_repeat1",
  [aux_sym_function_clauses_repeat1] = "function_clauses_repeat1",
  [aux_sym_exprs_repeat1] = "exprs_repeat1",
  [aux_sym_strings_repeat1] = "strings_repeat1",
  [aux_sym_string_repeat1] = "string_repeat1",
  [anon_alias_sym_function_clause_exprs] = "function_clause_exprs",
};

static TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_atom] = sym_atom,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_module] = anon_sym_module,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_export] = anon_sym_export,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [anon_sym_when] = anon_sym_when,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_bor] = anon_sym_bor,
  [anon_sym_bxor] = anon_sym_bxor,
  [anon_sym_bsl] = anon_sym_bsl,
  [anon_sym_bsr] = anon_sym_bsr,
  [anon_sym_or] = anon_sym_or,
  [anon_sym_xor] = anon_sym_xor,
  [sym_variable] = sym_variable,
  [sym_integer] = sym_integer,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [sym__escape_sequence] = sym__escape_sequence,
  [sym_comment] = sym_comment,
  [sym_line_comment] = sym_line_comment,
  [sym__newline] = sym__newline,
  [sym_multiple_newlines] = sym_multiple_newlines,
  [sym__spaces] = sym__spaces,
  [sym_source_file] = sym_source_file,
  [sym_form] = sym_form,
  [sym__attribute] = sym__attribute,
  [sym_module_attribute] = sym_module_attribute,
  [sym_export_attribute] = sym_export_attribute,
  [sym_export_attribute_block] = sym_export_attribute_block,
  [sym_export_attribute_mfas] = sym_export_attribute_mfas,
  [sym_export_attribute_mfa] = sym_export_attribute_mfa,
  [sym__function_or_macro] = sym__function_or_macro,
  [sym_function_clauses_trailing_semicolon] = sym_function_clauses_trailing_semicolon,
  [sym_function_clauses] = sym_function_clauses,
  [sym_function_clause_block] = sym_function_clause_block,
  [sym_function_clause_open] = sym_function_clause_open,
  [sym_pat_argument_list] = sym_pat_argument_list,
  [sym_clause_guard] = sym_clause_guard,
  [sym_function_clause_exprs_trailing_comma] = sym_function_clause_exprs_trailing_comma,
  [sym_exprs] = sym_exprs,
  [sym_expr] = sym_expr,
  [sym_function_call_block] = sym_function_call_block,
  [sym_function_call_open] = sym_function_call_open,
  [sym_remote_expr] = sym_remote_expr,
  [sym_expr_max] = sym_expr_max,
  [sym_equal_op] = sym_equal_op,
  [sym_add_op] = sym_add_op,
  [sym__atomic] = sym__atomic,
  [sym_strings] = sym_strings,
  [sym_string] = sym_string,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_export_attribute_mfas_repeat1] = aux_sym_export_attribute_mfas_repeat1,
  [aux_sym_function_clauses_repeat1] = aux_sym_function_clauses_repeat1,
  [aux_sym_exprs_repeat1] = aux_sym_exprs_repeat1,
  [aux_sym_strings_repeat1] = aux_sym_strings_repeat1,
  [aux_sym_string_repeat1] = aux_sym_string_repeat1,
  [anon_alias_sym_function_clause_exprs] = anon_alias_sym_function_clause_exprs,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_atom] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_module] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_export] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_when] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_bor] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_bxor] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_bsl] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_bsr] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_or] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_xor] = {
    .visible = true,
    .named = false,
  },
  [sym_variable] = {
    .visible = true,
    .named = true,
  },
  [sym_integer] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_token1] = {
    .visible = false,
    .named = false,
  },
  [sym__escape_sequence] = {
    .visible = false,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_line_comment] = {
    .visible = true,
    .named = true,
  },
  [sym__newline] = {
    .visible = false,
    .named = true,
  },
  [sym_multiple_newlines] = {
    .visible = true,
    .named = true,
  },
  [sym__spaces] = {
    .visible = false,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_form] = {
    .visible = true,
    .named = true,
  },
  [sym__attribute] = {
    .visible = false,
    .named = true,
  },
  [sym_module_attribute] = {
    .visible = true,
    .named = true,
  },
  [sym_export_attribute] = {
    .visible = true,
    .named = true,
  },
  [sym_export_attribute_block] = {
    .visible = true,
    .named = true,
  },
  [sym_export_attribute_mfas] = {
    .visible = true,
    .named = true,
  },
  [sym_export_attribute_mfa] = {
    .visible = true,
    .named = true,
  },
  [sym__function_or_macro] = {
    .visible = false,
    .named = true,
  },
  [sym_function_clauses_trailing_semicolon] = {
    .visible = true,
    .named = true,
  },
  [sym_function_clauses] = {
    .visible = true,
    .named = true,
  },
  [sym_function_clause_block] = {
    .visible = true,
    .named = true,
  },
  [sym_function_clause_open] = {
    .visible = true,
    .named = true,
  },
  [sym_pat_argument_list] = {
    .visible = true,
    .named = true,
  },
  [sym_clause_guard] = {
    .visible = true,
    .named = true,
  },
  [sym_function_clause_exprs_trailing_comma] = {
    .visible = true,
    .named = true,
  },
  [sym_exprs] = {
    .visible = true,
    .named = true,
  },
  [sym_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_function_call_block] = {
    .visible = true,
    .named = true,
  },
  [sym_function_call_open] = {
    .visible = true,
    .named = true,
  },
  [sym_remote_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_expr_max] = {
    .visible = true,
    .named = true,
  },
  [sym_equal_op] = {
    .visible = true,
    .named = true,
  },
  [sym_add_op] = {
    .visible = true,
    .named = true,
  },
  [sym__atomic] = {
    .visible = false,
    .named = true,
  },
  [sym_strings] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_export_attribute_mfas_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_function_clauses_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_exprs_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_strings_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [anon_alias_sym_function_clause_exprs] = {
    .visible = true,
    .named = false,
  },
};

static TSSymbol ts_alias_sequences[2][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [1] = {
    [0] = anon_alias_sym_function_clause_exprs,
  },
};

static uint16_t ts_non_terminal_alias_map[] = {
  sym_exprs, 2,
    sym_exprs,
    anon_alias_sym_function_clause_exprs,
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(11);
      if (lookahead == '\n') ADVANCE(33);
      if (lookahead == '"') ADVANCE(32);
      if (lookahead == '%') ADVANCE(35);
      if (lookahead == '\'') ADVANCE(36);
      if (lookahead == '(') ADVANCE(40);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == '+') ADVANCE(40);
      if (lookahead == ',') ADVANCE(40);
      if (lookahead == '-') ADVANCE(37);
      if (lookahead == '.') ADVANCE(40);
      if (lookahead == '/') ADVANCE(40);
      if (lookahead == ':') ADVANCE(40);
      if (lookahead == ';') ADVANCE(40);
      if (lookahead == '=') ADVANCE(40);
      if (lookahead == '[') ADVANCE(40);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == ']') ADVANCE(40);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(40);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(38);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(39);
      if (lookahead != 0) ADVANCE(40);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(33);
      if (lookahead == '"') ADVANCE(32);
      if (lookahead == '%') ADVANCE(35);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(40);
      if (lookahead != 0) ADVANCE(40);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(46);
      if (lookahead == '"') ADVANCE(32);
      if (lookahead == '%') ADVANCE(3);
      if (lookahead == '\'') ADVANCE(4);
      if (lookahead == ')') ADVANCE(15);
      if (lookahead == ',') ADVANCE(17);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(48);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(29);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(25);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(45);
      if (lookahead != 0) ADVANCE(44);
      END_STATE();
    case 4:
      if (lookahead == '\'') ADVANCE(26);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(21);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(29);
      END_STATE();
    case 6:
      if (lookahead == 'x') ADVANCE(7);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(43);
      if (lookahead != 0 &&
          lookahead != 'U' &&
          lookahead != 'u') ADVANCE(41);
      END_STATE();
    case 7:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(41);
      END_STATE();
    case 8:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(31);
      END_STATE();
    case 9:
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(4);
      if (lookahead == '\'') ADVANCE(27);
      if (lookahead == '\\') ADVANCE(9);
      END_STATE();
    case 10:
      if (eof) ADVANCE(11);
      if (lookahead == '\n') ADVANCE(46);
      if (lookahead == '"') ADVANCE(32);
      if (lookahead == '%') ADVANCE(3);
      if (lookahead == '\'') ADVANCE(4);
      if (lookahead == '(') ADVANCE(14);
      if (lookahead == ')') ADVANCE(15);
      if (lookahead == '+') ADVANCE(24);
      if (lookahead == ',') ADVANCE(17);
      if (lookahead == '-') ADVANCE(13);
      if (lookahead == '.') ADVANCE(12);
      if (lookahead == '/') ADVANCE(19);
      if (lookahead == ':') ADVANCE(22);
      if (lookahead == ';') ADVANCE(20);
      if (lookahead == '=') ADVANCE(23);
      if (lookahead == '[') ADVANCE(16);
      if (lookahead == ']') ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(48);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_atom);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(sym_atom);
      if (lookahead == '\'') ADVANCE(26);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(sym_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '#') ADVANCE(8);
      if (lookahead == '_') ADVANCE(30);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(29);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(33);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(34);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(34);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(34);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\'') ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(36);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '>') ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(38);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(39);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym__escape_sequence);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(41);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(42);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(44);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym_line_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(45);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(sym__newline);
      if (lookahead == '\n') ADVANCE(47);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym_multiple_newlines);
      if (lookahead == '\n') ADVANCE(47);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym__spaces);
      END_STATE();
    default:
      return false;
  }
}

static bool ts_lex_keywords(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (lookahead == 'b') ADVANCE(1);
      if (lookahead == 'e') ADVANCE(2);
      if (lookahead == 'm') ADVANCE(3);
      if (lookahead == 'o') ADVANCE(4);
      if (lookahead == 'w') ADVANCE(5);
      if (lookahead == 'x') ADVANCE(6);
      END_STATE();
    case 1:
      if (lookahead == 'o') ADVANCE(7);
      if (lookahead == 's') ADVANCE(8);
      if (lookahead == 'x') ADVANCE(9);
      END_STATE();
    case 2:
      if (lookahead == 'x') ADVANCE(10);
      END_STATE();
    case 3:
      if (lookahead == 'o') ADVANCE(11);
      END_STATE();
    case 4:
      if (lookahead == 'r') ADVANCE(12);
      END_STATE();
    case 5:
      if (lookahead == 'h') ADVANCE(13);
      END_STATE();
    case 6:
      if (lookahead == 'o') ADVANCE(14);
      END_STATE();
    case 7:
      if (lookahead == 'r') ADVANCE(15);
      END_STATE();
    case 8:
      if (lookahead == 'l') ADVANCE(16);
      if (lookahead == 'r') ADVANCE(17);
      END_STATE();
    case 9:
      if (lookahead == 'o') ADVANCE(18);
      END_STATE();
    case 10:
      if (lookahead == 'p') ADVANCE(19);
      END_STATE();
    case 11:
      if (lookahead == 'd') ADVANCE(20);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(21);
      END_STATE();
    case 14:
      if (lookahead == 'r') ADVANCE(22);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_bor);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_bsl);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_bsr);
      END_STATE();
    case 18:
      if (lookahead == 'r') ADVANCE(23);
      END_STATE();
    case 19:
      if (lookahead == 'o') ADVANCE(24);
      END_STATE();
    case 20:
      if (lookahead == 'u') ADVANCE(25);
      END_STATE();
    case 21:
      if (lookahead == 'n') ADVANCE(26);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_xor);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_bxor);
      END_STATE();
    case 24:
      if (lookahead == 'r') ADVANCE(27);
      END_STATE();
    case 25:
      if (lookahead == 'l') ADVANCE(28);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(anon_sym_when);
      END_STATE();
    case 27:
      if (lookahead == 't') ADVANCE(29);
      END_STATE();
    case 28:
      if (lookahead == 'e') ADVANCE(30);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_export);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_module);
      END_STATE();
    default:
      return false;
  }
}

static TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 10},
  [2] = {.lex_state = 10},
  [3] = {.lex_state = 10},
  [4] = {.lex_state = 10},
  [5] = {.lex_state = 2},
  [6] = {.lex_state = 10},
  [7] = {.lex_state = 10},
  [8] = {.lex_state = 10},
  [9] = {.lex_state = 10},
  [10] = {.lex_state = 10},
  [11] = {.lex_state = 10},
  [12] = {.lex_state = 2},
  [13] = {.lex_state = 10},
  [14] = {.lex_state = 10},
  [15] = {.lex_state = 10},
  [16] = {.lex_state = 2},
  [17] = {.lex_state = 10},
  [18] = {.lex_state = 2},
  [19] = {.lex_state = 2},
  [20] = {.lex_state = 10},
  [21] = {.lex_state = 10},
  [22] = {.lex_state = 10},
  [23] = {.lex_state = 10},
  [24] = {.lex_state = 10},
  [25] = {.lex_state = 2},
  [26] = {.lex_state = 2},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 10},
  [29] = {.lex_state = 10},
  [30] = {.lex_state = 2},
  [31] = {.lex_state = 1},
  [32] = {.lex_state = 2},
  [33] = {.lex_state = 1},
  [34] = {.lex_state = 2},
  [35] = {.lex_state = 2},
  [36] = {.lex_state = 1},
  [37] = {.lex_state = 10},
  [38] = {.lex_state = 10},
  [39] = {.lex_state = 10},
  [40] = {.lex_state = 10},
  [41] = {.lex_state = 10},
  [42] = {.lex_state = 2},
  [43] = {.lex_state = 10},
  [44] = {.lex_state = 10},
  [45] = {.lex_state = 10},
  [46] = {.lex_state = 10},
  [47] = {.lex_state = 10},
  [48] = {.lex_state = 10},
  [49] = {.lex_state = 10},
  [50] = {.lex_state = 10},
  [51] = {.lex_state = 10},
  [52] = {.lex_state = 10},
  [53] = {.lex_state = 10},
  [54] = {.lex_state = 2},
  [55] = {.lex_state = 10},
  [56] = {.lex_state = 10},
  [57] = {.lex_state = 10},
  [58] = {.lex_state = 10},
  [59] = {.lex_state = 10},
  [60] = {.lex_state = 10},
  [61] = {.lex_state = 10},
  [62] = {.lex_state = 10},
  [63] = {.lex_state = 10},
  [64] = {.lex_state = 10},
  [65] = {.lex_state = 10},
  [66] = {.lex_state = 10},
  [67] = {.lex_state = 10},
  [68] = {.lex_state = 10},
  [69] = {.lex_state = 10},
  [70] = {.lex_state = 2},
  [71] = {.lex_state = 10},
  [72] = {.lex_state = 10},
  [73] = {.lex_state = 10},
  [74] = {.lex_state = 2},
  [75] = {.lex_state = 10},
  [76] = {.lex_state = 10},
  [77] = {.lex_state = 10},
  [78] = {.lex_state = 10},
  [79] = {.lex_state = 10},
  [80] = {.lex_state = 10},
  [81] = {.lex_state = 2},
};

static uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_atom] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_module] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_export] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_when] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_bor] = ACTIONS(1),
    [anon_sym_bxor] = ACTIONS(1),
    [anon_sym_bsl] = ACTIONS(1),
    [anon_sym_bsr] = ACTIONS(1),
    [anon_sym_or] = ACTIONS(1),
    [anon_sym_xor] = ACTIONS(1),
    [sym_variable] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [aux_sym_string_token1] = ACTIONS(1),
    [sym__escape_sequence] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [sym_line_comment] = ACTIONS(3),
    [sym__newline] = ACTIONS(3),
    [sym_multiple_newlines] = ACTIONS(3),
    [sym__spaces] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(64),
    [sym_form] = STATE(22),
    [sym__attribute] = STATE(62),
    [sym_module_attribute] = STATE(62),
    [sym_export_attribute] = STATE(62),
    [sym__function_or_macro] = STATE(62),
    [sym_function_clauses_trailing_semicolon] = STATE(62),
    [sym_function_clauses] = STATE(59),
    [sym_function_clause_block] = STATE(39),
    [sym_function_clause_open] = STATE(12),
    [aux_sym_source_file_repeat1] = STATE(22),
    [sym_atom] = ACTIONS(5),
    [anon_sym_DASH] = ACTIONS(7),
    [sym_comment] = ACTIONS(9),
    [sym_line_comment] = ACTIONS(9),
    [sym__newline] = ACTIONS(3),
    [sym_multiple_newlines] = ACTIONS(9),
    [sym__spaces] = ACTIONS(9),
  },
};

static uint16_t ts_small_parse_table[] = {
  [0] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    STATE(3), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(11), 15,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [34] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(3), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(15), 15,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [68] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(20), 16,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
      anon_sym_DQUOTE,
  [96] = 14,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(24), 1,
      anon_sym_RPAREN,
    ACTIONS(26), 1,
      anon_sym_COMMA,
    STATE(5), 1,
      sym_function_call_open,
    STATE(6), 1,
      sym_expr,
    STATE(9), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_function_call_block,
    STATE(48), 1,
      sym_exprs,
    STATE(2), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(8), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(22), 3,
      sym_variable,
      sym_atom,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [146] = 9,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(32), 1,
      anon_sym_COMMA,
    ACTIONS(35), 1,
      anon_sym_EQ,
    STATE(18), 1,
      sym_add_op,
    STATE(19), 1,
      sym_equal_op,
    STATE(27), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(28), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(30), 8,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [186] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(37), 16,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
      anon_sym_DQUOTE,
  [214] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(39), 15,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [241] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(43), 1,
      anon_sym_COLON,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(41), 14,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [270] = 6,
    ACTIONS(3), 1,
      sym__newline,
    STATE(18), 1,
      sym_add_op,
    STATE(19), 1,
      sym_equal_op,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(45), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
    ACTIONS(30), 8,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [303] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(35), 1,
      anon_sym_EQ,
    STATE(18), 1,
      sym_add_op,
    STATE(19), 1,
      sym_equal_op,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(45), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
    ACTIONS(30), 8,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [338] = 13,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    STATE(5), 1,
      sym_function_call_open,
    STATE(6), 1,
      sym_expr,
    STATE(9), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_function_call_block,
    STATE(43), 1,
      sym_exprs,
    STATE(53), 1,
      sym_function_clause_exprs_trailing_comma,
    STATE(2), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(8), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(22), 3,
      sym_variable,
      sym_atom,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [385] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(35), 1,
      anon_sym_EQ,
    STATE(18), 1,
      sym_add_op,
    STATE(19), 1,
      sym_equal_op,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(47), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
    ACTIONS(30), 8,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [420] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(49), 14,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [446] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(53), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(51), 13,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [474] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    STATE(5), 1,
      sym_function_call_open,
    STATE(9), 1,
      sym_expr_max,
    STATE(13), 1,
      sym_expr,
    STATE(15), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_function_call_block,
    STATE(2), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(8), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(22), 3,
      sym_variable,
      sym_atom,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [515] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(55), 13,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [540] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    STATE(5), 1,
      sym_function_call_open,
    STATE(9), 1,
      sym_expr_max,
    STATE(10), 1,
      sym_expr,
    STATE(15), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_function_call_block,
    STATE(2), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(8), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(22), 3,
      sym_variable,
      sym_atom,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [581] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    STATE(5), 1,
      sym_function_call_open,
    STATE(9), 1,
      sym_expr_max,
    STATE(11), 1,
      sym_expr,
    STATE(15), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_function_call_block,
    STATE(2), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(8), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(22), 3,
      sym_variable,
      sym_atom,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [622] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(57), 13,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [647] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(51), 13,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [672] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(59), 1,
      ts_builtin_sym_end,
    STATE(12), 1,
      sym_function_clause_open,
    STATE(39), 1,
      sym_function_clause_block,
    STATE(59), 1,
      sym_function_clauses,
    STATE(23), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(62), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [711] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(61), 1,
      ts_builtin_sym_end,
    ACTIONS(63), 1,
      sym_atom,
    ACTIONS(66), 1,
      anon_sym_DASH,
    STATE(12), 1,
      sym_function_clause_open,
    STATE(39), 1,
      sym_function_clause_block,
    STATE(59), 1,
      sym_function_clauses,
    STATE(23), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(62), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [750] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(69), 13,
      anon_sym_DOT,
      anon_sym_DASH,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_PLUS,
      anon_sym_bor,
      anon_sym_bxor,
      anon_sym_bsl,
      anon_sym_bsr,
      anon_sym_or,
      anon_sym_xor,
  [775] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    STATE(14), 1,
      sym_expr_max,
    STATE(2), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(8), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(22), 3,
      sym_variable,
      sym_atom,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [804] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(71), 6,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      sym_variable,
      sym_atom,
      sym_integer,
      anon_sym_DQUOTE,
  [822] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(75), 1,
      anon_sym_COMMA,
    STATE(29), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(73), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [843] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(78), 1,
      sym_atom,
    ACTIONS(80), 1,
      anon_sym_COMMA,
    ACTIONS(82), 1,
      anon_sym_RBRACK,
    STATE(41), 1,
      sym_export_attribute_mfa,
    STATE(51), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [868] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(84), 1,
      anon_sym_COMMA,
    STATE(29), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(47), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [889] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(87), 4,
      sym_variable,
      sym_atom,
      sym_integer,
      anon_sym_DQUOTE,
  [905] = 4,
    ACTIONS(89), 1,
      anon_sym_DQUOTE,
    STATE(36), 1,
      aux_sym_string_repeat1,
    ACTIONS(91), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [923] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(93), 4,
      sym_variable,
      sym_atom,
      sym_integer,
      anon_sym_DQUOTE,
  [939] = 4,
    ACTIONS(95), 1,
      anon_sym_DQUOTE,
    STATE(33), 1,
      aux_sym_string_repeat1,
    ACTIONS(97), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [957] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(100), 4,
      sym_variable,
      sym_atom,
      sym_integer,
      anon_sym_DQUOTE,
  [973] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(102), 4,
      sym_variable,
      sym_atom,
      sym_integer,
      anon_sym_DQUOTE,
  [989] = 4,
    ACTIONS(104), 1,
      anon_sym_DQUOTE,
    STATE(33), 1,
      aux_sym_string_repeat1,
    ACTIONS(106), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [1007] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(108), 1,
      anon_sym_COMMA,
    ACTIONS(111), 1,
      anon_sym_RBRACK,
    STATE(40), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1026] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(113), 1,
      anon_sym_DOT,
    ACTIONS(115), 1,
      anon_sym_SEMI,
    STATE(38), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1045] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(118), 1,
      anon_sym_DOT,
    ACTIONS(120), 1,
      anon_sym_SEMI,
    STATE(46), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1064] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(123), 1,
      anon_sym_COMMA,
    ACTIONS(126), 1,
      anon_sym_RBRACK,
    STATE(40), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1083] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(128), 1,
      anon_sym_COMMA,
    ACTIONS(131), 1,
      anon_sym_RBRACK,
    STATE(37), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1102] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(133), 1,
      anon_sym_DASH_GT,
    ACTIONS(135), 1,
      anon_sym_when,
    STATE(81), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1121] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(139), 1,
      anon_sym_COMMA,
    ACTIONS(137), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1138] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(141), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1153] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(12), 1,
      sym_function_clause_open,
    STATE(47), 1,
      sym_function_clause_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1172] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(143), 1,
      anon_sym_DOT,
    ACTIONS(145), 1,
      anon_sym_SEMI,
    STATE(38), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1191] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(113), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1205] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 1,
      anon_sym_RPAREN,
    ACTIONS(150), 1,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1221] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(152), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1235] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(78), 1,
      sym_atom,
    STATE(55), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1251] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 1,
      anon_sym_COMMA,
    ACTIONS(156), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1267] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 1,
      anon_sym_LPAREN,
    STATE(42), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1283] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1297] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1311] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(126), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1325] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(164), 1,
      anon_sym_module,
    ACTIONS(166), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1341] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(168), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1355] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(170), 1,
      anon_sym_LBRACK,
    STATE(60), 1,
      sym_export_attribute_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1371] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(172), 1,
      anon_sym_DOT,
    ACTIONS(174), 1,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1387] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(176), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1400] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(178), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1413] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(180), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1426] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(182), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1439] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(184), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1452] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(186), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1465] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1478] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(188), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1491] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(190), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1504] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(192), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1517] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(194), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1530] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(196), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1543] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(198), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1556] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(200), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1569] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(202), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1582] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(204), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1595] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(206), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1608] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(208), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1621] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1634] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(210), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1647] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(212), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1660] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(214), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 34,
  [SMALL_STATE(4)] = 68,
  [SMALL_STATE(5)] = 96,
  [SMALL_STATE(6)] = 146,
  [SMALL_STATE(7)] = 186,
  [SMALL_STATE(8)] = 214,
  [SMALL_STATE(9)] = 241,
  [SMALL_STATE(10)] = 270,
  [SMALL_STATE(11)] = 303,
  [SMALL_STATE(12)] = 338,
  [SMALL_STATE(13)] = 385,
  [SMALL_STATE(14)] = 420,
  [SMALL_STATE(15)] = 446,
  [SMALL_STATE(16)] = 474,
  [SMALL_STATE(17)] = 515,
  [SMALL_STATE(18)] = 540,
  [SMALL_STATE(19)] = 581,
  [SMALL_STATE(20)] = 622,
  [SMALL_STATE(21)] = 647,
  [SMALL_STATE(22)] = 672,
  [SMALL_STATE(23)] = 711,
  [SMALL_STATE(24)] = 750,
  [SMALL_STATE(25)] = 775,
  [SMALL_STATE(26)] = 804,
  [SMALL_STATE(27)] = 822,
  [SMALL_STATE(28)] = 843,
  [SMALL_STATE(29)] = 868,
  [SMALL_STATE(30)] = 889,
  [SMALL_STATE(31)] = 905,
  [SMALL_STATE(32)] = 923,
  [SMALL_STATE(33)] = 939,
  [SMALL_STATE(34)] = 957,
  [SMALL_STATE(35)] = 973,
  [SMALL_STATE(36)] = 989,
  [SMALL_STATE(37)] = 1007,
  [SMALL_STATE(38)] = 1026,
  [SMALL_STATE(39)] = 1045,
  [SMALL_STATE(40)] = 1064,
  [SMALL_STATE(41)] = 1083,
  [SMALL_STATE(42)] = 1102,
  [SMALL_STATE(43)] = 1121,
  [SMALL_STATE(44)] = 1138,
  [SMALL_STATE(45)] = 1153,
  [SMALL_STATE(46)] = 1172,
  [SMALL_STATE(47)] = 1191,
  [SMALL_STATE(48)] = 1205,
  [SMALL_STATE(49)] = 1221,
  [SMALL_STATE(50)] = 1235,
  [SMALL_STATE(51)] = 1251,
  [SMALL_STATE(52)] = 1267,
  [SMALL_STATE(53)] = 1283,
  [SMALL_STATE(54)] = 1297,
  [SMALL_STATE(55)] = 1311,
  [SMALL_STATE(56)] = 1325,
  [SMALL_STATE(57)] = 1341,
  [SMALL_STATE(58)] = 1355,
  [SMALL_STATE(59)] = 1371,
  [SMALL_STATE(60)] = 1387,
  [SMALL_STATE(61)] = 1400,
  [SMALL_STATE(62)] = 1413,
  [SMALL_STATE(63)] = 1426,
  [SMALL_STATE(64)] = 1439,
  [SMALL_STATE(65)] = 1452,
  [SMALL_STATE(66)] = 1465,
  [SMALL_STATE(67)] = 1478,
  [SMALL_STATE(68)] = 1491,
  [SMALL_STATE(69)] = 1504,
  [SMALL_STATE(70)] = 1517,
  [SMALL_STATE(71)] = 1530,
  [SMALL_STATE(72)] = 1543,
  [SMALL_STATE(73)] = 1556,
  [SMALL_STATE(74)] = 1569,
  [SMALL_STATE(75)] = 1582,
  [SMALL_STATE(76)] = 1595,
  [SMALL_STATE(77)] = 1608,
  [SMALL_STATE(78)] = 1621,
  [SMALL_STATE(79)] = 1634,
  [SMALL_STATE(80)] = 1647,
  [SMALL_STATE(81)] = 1660,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [17] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(31),
  [20] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [22] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [24] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [26] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [28] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [30] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [32] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 1), SHIFT(16),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_remote_expr, 1),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 3),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_remote_expr, 3),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 2),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 4),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(52),
  [66] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(56),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 3),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_open, 2),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [75] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 2), SHIFT(16),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [84] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(16),
  [87] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_add_op, 1),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 3),
  [95] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [97] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(33),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 4),
  [102] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equal_op, 1),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [108] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2), SHIFT(50),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [113] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2),
  [115] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2), SHIFT_REPEAT(45),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 1),
  [120] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 1), SHIFT(45),
  [123] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(50),
  [126] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [128] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1), SHIFT(50),
  [131] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 1, .production_id = 1),
  [139] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [141] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [143] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 2),
  [145] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 2), SHIFT(45),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 2, .production_id = 1),
  [154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_block, 2),
  [162] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [168] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [170] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [172] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 1),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [182] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [184] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [188] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 2),
  [190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [194] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [198] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [200] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 3),
  [202] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [206] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [208] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 2),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 4),
  [212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [214] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_erlang(void) {
  static TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .parse_table = (const uint16_t *)ts_parse_table,
    .parse_actions = ts_parse_actions,
    .lex_modes = ts_lex_modes,
    .alias_sequences = (const TSSymbol *)ts_alias_sequences,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym_atom,
    .field_count = FIELD_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .small_parse_table = (const uint16_t *)ts_small_parse_table,
    .small_parse_table_map = (const uint32_t *)ts_small_parse_table_map,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .state_count = STATE_COUNT,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
