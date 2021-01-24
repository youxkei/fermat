#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 81
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 61
#define ALIAS_COUNT 1
#define TOKEN_COUNT 28
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
  anon_sym_catch = 15,
  anon_sym_EQ = 16,
  anon_sym_COLON = 17,
  sym_variable = 18,
  sym_integer = 19,
  anon_sym_DQUOTE = 20,
  aux_sym_string_token1 = 21,
  sym__escape_sequence = 22,
  sym_comment = 23,
  sym_line_comment = 24,
  sym__newline = 25,
  sym_multiple_newlines = 26,
  sym__spaces = 27,
  sym_source_file = 28,
  sym_form = 29,
  sym__attribute = 30,
  sym_module_attribute = 31,
  sym_export_attribute = 32,
  sym_export_attribute_block = 33,
  sym_export_attribute_mfas = 34,
  sym_export_attribute_mfa = 35,
  sym__function_or_macro = 36,
  sym_function_clauses_trailing_semicolon = 37,
  sym_function_clauses = 38,
  sym_function_clause_block = 39,
  sym_function_clause_open = 40,
  sym_pat_argument_list = 41,
  sym_clause_guard = 42,
  sym_function_clause_exprs_trailing_comma = 43,
  sym_exprs = 44,
  sym_expr = 45,
  sym_equal_op_expr_block = 46,
  sym_equal_op_expr_open = 47,
  sym_function_call_block = 48,
  sym_function_call_open = 49,
  sym_remote_expr = 50,
  sym_expr_max = 51,
  sym__atomic = 52,
  sym_strings = 53,
  sym_string = 54,
  aux_sym_source_file_repeat1 = 55,
  aux_sym_export_attribute_mfas_repeat1 = 56,
  aux_sym_function_clauses_repeat1 = 57,
  aux_sym_exprs_repeat1 = 58,
  aux_sym_strings_repeat1 = 59,
  aux_sym_string_repeat1 = 60,
  anon_alias_sym_function_clause_exprs = 61,
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
  [anon_sym_catch] = "catch",
  [anon_sym_EQ] = "=",
  [anon_sym_COLON] = ":",
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
  [sym_equal_op_expr_block] = "equal_op_expr_block",
  [sym_equal_op_expr_open] = "equal_op_expr_open",
  [sym_function_call_block] = "function_call_block",
  [sym_function_call_open] = "function_call_open",
  [sym_remote_expr] = "remote_expr",
  [sym_expr_max] = "expr_max",
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
  [anon_sym_catch] = anon_sym_catch,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_COLON] = anon_sym_COLON,
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
  [sym_equal_op_expr_block] = sym_equal_op_expr_block,
  [sym_equal_op_expr_open] = sym_equal_op_expr_open,
  [sym_function_call_block] = sym_function_call_block,
  [sym_function_call_open] = sym_function_call_open,
  [sym_remote_expr] = sym_remote_expr,
  [sym_expr_max] = sym_expr_max,
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
  [anon_sym_catch] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
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
  [sym_equal_op_expr_block] = {
    .visible = true,
    .named = true,
  },
  [sym_equal_op_expr_open] = {
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
      if (lookahead == '\n') ADVANCE(32);
      if (lookahead == '"') ADVANCE(31);
      if (lookahead == '%') ADVANCE(35);
      if (lookahead == '\'') ADVANCE(36);
      if (lookahead == '(') ADVANCE(43);
      if (lookahead == ')') ADVANCE(43);
      if (lookahead == ',') ADVANCE(43);
      if (lookahead == '-') ADVANCE(37);
      if (lookahead == '.') ADVANCE(43);
      if (lookahead == '/') ADVANCE(43);
      if (lookahead == ':') ADVANCE(43);
      if (lookahead == ';') ADVANCE(43);
      if (lookahead == '=') ADVANCE(43);
      if (lookahead == '[') ADVANCE(43);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == ']') ADVANCE(43);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(43);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(34);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(41);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(42);
      if (lookahead != 0) ADVANCE(43);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(32);
      if (lookahead == '"') ADVANCE(31);
      if (lookahead == '%') ADVANCE(35);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(43);
      if (lookahead != 0) ADVANCE(43);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == '%') ADVANCE(3);
      if (lookahead == '\'') ADVANCE(4);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(51);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(27);
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(48);
      if (lookahead != 0) ADVANCE(47);
      END_STATE();
    case 4:
      if (lookahead == '\'') ADVANCE(25);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(21);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      END_STATE();
    case 6:
      if (lookahead == 'x') ADVANCE(7);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(46);
      if (lookahead != 0 &&
          lookahead != 'U' &&
          lookahead != 'u') ADVANCE(44);
      END_STATE();
    case 7:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(44);
      END_STATE();
    case 8:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 9:
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(4);
      if (lookahead == '\'') ADVANCE(26);
      if (lookahead == '\\') ADVANCE(9);
      END_STATE();
    case 10:
      if (eof) ADVANCE(11);
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == '"') ADVANCE(31);
      if (lookahead == '%') ADVANCE(3);
      if (lookahead == '\'') ADVANCE(4);
      if (lookahead == '(') ADVANCE(14);
      if (lookahead == ')') ADVANCE(15);
      if (lookahead == ',') ADVANCE(17);
      if (lookahead == '-') ADVANCE(13);
      if (lookahead == '.') ADVANCE(12);
      if (lookahead == '/') ADVANCE(19);
      if (lookahead == ':') ADVANCE(23);
      if (lookahead == ';') ADVANCE(20);
      if (lookahead == '=') ADVANCE(22);
      if (lookahead == '[') ADVANCE(16);
      if (lookahead == ']') ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(51);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(24);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(27);
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
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_atom);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_atom);
      if (lookahead == '\'') ADVANCE(25);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(sym_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(27);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '#') ADVANCE(8);
      if (lookahead == '_') ADVANCE(29);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(29);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(32);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(43);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(33);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '#') ADVANCE(39);
      if (lookahead == '_') ADVANCE(38);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(34);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(33);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(33);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\'') ADVANCE(43);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(36);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '>') ADVANCE(43);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(34);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(38);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(42);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(43);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym__escape_sequence);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(44);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(45);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(47);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym_line_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(48);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(sym__newline);
      if (lookahead == '\n') ADVANCE(50);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(sym_multiple_newlines);
      if (lookahead == '\n') ADVANCE(50);
      END_STATE();
    case 51:
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
      if (lookahead == 'c') ADVANCE(1);
      if (lookahead == 'e') ADVANCE(2);
      if (lookahead == 'm') ADVANCE(3);
      if (lookahead == 'w') ADVANCE(4);
      END_STATE();
    case 1:
      if (lookahead == 'a') ADVANCE(5);
      END_STATE();
    case 2:
      if (lookahead == 'x') ADVANCE(6);
      END_STATE();
    case 3:
      if (lookahead == 'o') ADVANCE(7);
      END_STATE();
    case 4:
      if (lookahead == 'h') ADVANCE(8);
      END_STATE();
    case 5:
      if (lookahead == 't') ADVANCE(9);
      END_STATE();
    case 6:
      if (lookahead == 'p') ADVANCE(10);
      END_STATE();
    case 7:
      if (lookahead == 'd') ADVANCE(11);
      END_STATE();
    case 8:
      if (lookahead == 'e') ADVANCE(12);
      END_STATE();
    case 9:
      if (lookahead == 'c') ADVANCE(13);
      END_STATE();
    case 10:
      if (lookahead == 'o') ADVANCE(14);
      END_STATE();
    case 11:
      if (lookahead == 'u') ADVANCE(15);
      END_STATE();
    case 12:
      if (lookahead == 'n') ADVANCE(16);
      END_STATE();
    case 13:
      if (lookahead == 'h') ADVANCE(17);
      END_STATE();
    case 14:
      if (lookahead == 'r') ADVANCE(18);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(19);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_when);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_catch);
      END_STATE();
    case 18:
      if (lookahead == 't') ADVANCE(20);
      END_STATE();
    case 19:
      if (lookahead == 'e') ADVANCE(21);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_export);
      END_STATE();
    case 21:
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
  [5] = {.lex_state = 10},
  [6] = {.lex_state = 10},
  [7] = {.lex_state = 10},
  [8] = {.lex_state = 10},
  [9] = {.lex_state = 10},
  [10] = {.lex_state = 10},
  [11] = {.lex_state = 10},
  [12] = {.lex_state = 10},
  [13] = {.lex_state = 10},
  [14] = {.lex_state = 10},
  [15] = {.lex_state = 10},
  [16] = {.lex_state = 10},
  [17] = {.lex_state = 10},
  [18] = {.lex_state = 10},
  [19] = {.lex_state = 10},
  [20] = {.lex_state = 10},
  [21] = {.lex_state = 10},
  [22] = {.lex_state = 10},
  [23] = {.lex_state = 10},
  [24] = {.lex_state = 10},
  [25] = {.lex_state = 10},
  [26] = {.lex_state = 10},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 10},
  [29] = {.lex_state = 10},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 1},
  [32] = {.lex_state = 10},
  [33] = {.lex_state = 1},
  [34] = {.lex_state = 10},
  [35] = {.lex_state = 10},
  [36] = {.lex_state = 10},
  [37] = {.lex_state = 10},
  [38] = {.lex_state = 2},
  [39] = {.lex_state = 10},
  [40] = {.lex_state = 10},
  [41] = {.lex_state = 10},
  [42] = {.lex_state = 10},
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
  [59] = {.lex_state = 2},
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
  [70] = {.lex_state = 10},
  [71] = {.lex_state = 10},
  [72] = {.lex_state = 10},
  [73] = {.lex_state = 2},
  [74] = {.lex_state = 10},
  [75] = {.lex_state = 10},
  [76] = {.lex_state = 10},
  [77] = {.lex_state = 2},
  [78] = {.lex_state = 10},
  [79] = {.lex_state = 10},
  [80] = {.lex_state = 10},
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
    [anon_sym_catch] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [sym_variable] = ACTIONS(1),
    [sym_integer] = ACTIONS(1),
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
    [sym_source_file] = STATE(60),
    [sym_form] = STATE(7),
    [sym__attribute] = STATE(61),
    [sym_module_attribute] = STATE(61),
    [sym_export_attribute] = STATE(61),
    [sym__function_or_macro] = STATE(61),
    [sym_function_clauses_trailing_semicolon] = STATE(61),
    [sym_function_clauses] = STATE(52),
    [sym_function_clause_block] = STATE(36),
    [sym_function_clause_open] = STATE(3),
    [aux_sym_source_file_repeat1] = STATE(7),
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
  [0] = 17,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(15), 1,
      anon_sym_COMMA,
    ACTIONS(17), 1,
      anon_sym_catch,
    ACTIONS(19), 1,
      sym_variable,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(5), 1,
      sym_equal_op_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(16), 1,
      sym_remote_expr,
    STATE(19), 1,
      sym_expr,
    STATE(49), 1,
      sym_exprs,
    STATE(10), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(15), 2,
      sym__atomic,
      sym_strings,
    STATE(29), 2,
      sym_equal_op_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [58] = 16,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_catch,
    ACTIONS(19), 1,
      sym_variable,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(5), 1,
      sym_equal_op_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(16), 1,
      sym_remote_expr,
    STATE(19), 1,
      sym_expr,
    STATE(43), 1,
      sym_exprs,
    STATE(50), 1,
      sym_function_clause_exprs_trailing_comma,
    STATE(10), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(15), 2,
      sym__atomic,
      sym_strings,
    STATE(29), 2,
      sym_equal_op_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [113] = 14,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_catch,
    ACTIONS(19), 1,
      sym_variable,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(5), 1,
      sym_equal_op_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(16), 1,
      sym_remote_expr,
    STATE(23), 1,
      sym_expr,
    STATE(10), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(15), 2,
      sym__atomic,
      sym_strings,
    STATE(29), 2,
      sym_equal_op_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [162] = 14,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_catch,
    ACTIONS(19), 1,
      sym_variable,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(5), 1,
      sym_equal_op_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(16), 1,
      sym_remote_expr,
    STATE(27), 1,
      sym_expr,
    STATE(10), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(15), 2,
      sym__atomic,
      sym_strings,
    STATE(29), 2,
      sym_equal_op_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [211] = 14,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_catch,
    ACTIONS(19), 1,
      sym_variable,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(5), 1,
      sym_equal_op_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(16), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_expr,
    STATE(10), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(15), 2,
      sym__atomic,
      sym_strings,
    STATE(29), 2,
      sym_equal_op_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [260] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(23), 1,
      ts_builtin_sym_end,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(36), 1,
      sym_function_clause_block,
    STATE(52), 1,
      sym_function_clauses,
    STATE(8), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(61), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [299] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(25), 1,
      ts_builtin_sym_end,
    ACTIONS(27), 1,
      sym_atom,
    ACTIONS(30), 1,
      anon_sym_DASH,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(36), 1,
      sym_function_clause_block,
    STATE(52), 1,
      sym_function_clauses,
    STATE(8), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(61), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [338] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(35), 1,
      anon_sym_DQUOTE,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(33), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
  [364] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(38), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
  [390] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(40), 8,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [410] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    STATE(17), 1,
      sym_expr_max,
    ACTIONS(19), 2,
      sym_variable,
      sym_atom,
    STATE(10), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(15), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [438] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(42), 8,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [458] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(46), 1,
      anon_sym_COLON,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(44), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [479] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(48), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
  [498] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(52), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(50), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [518] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(54), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [536] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(56), 2,
      anon_sym_catch,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(58), 4,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      sym_variable,
      anon_sym_DQUOTE,
  [556] = 6,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(62), 1,
      anon_sym_COMMA,
    ACTIONS(65), 1,
      anon_sym_EQ,
    STATE(28), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(60), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [580] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(69), 1,
      anon_sym_COMMA,
    STATE(20), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(67), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [601] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(72), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [618] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(74), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [635] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(65), 1,
      anon_sym_EQ,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(67), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [654] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(76), 1,
      sym_atom,
    ACTIONS(78), 1,
      anon_sym_COMMA,
    ACTIONS(80), 1,
      anon_sym_RBRACK,
    STATE(44), 1,
      sym_export_attribute_mfa,
    STATE(57), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [679] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(82), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [696] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(84), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [713] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(86), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [730] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(90), 1,
      anon_sym_COMMA,
    STATE(20), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(88), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [751] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(50), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [768] = 4,
    ACTIONS(93), 1,
      anon_sym_DQUOTE,
    STATE(31), 1,
      aux_sym_string_repeat1,
    ACTIONS(95), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [786] = 4,
    ACTIONS(97), 1,
      anon_sym_DQUOTE,
    STATE(31), 1,
      aux_sym_string_repeat1,
    ACTIONS(99), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [804] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(102), 2,
      anon_sym_catch,
      sym_atom,
    ACTIONS(104), 2,
      sym_variable,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [822] = 4,
    ACTIONS(106), 1,
      anon_sym_DQUOTE,
    STATE(30), 1,
      aux_sym_string_repeat1,
    ACTIONS(108), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [840] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(110), 2,
      anon_sym_catch,
      sym_atom,
    ACTIONS(112), 2,
      sym_variable,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [858] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(114), 2,
      anon_sym_catch,
      sym_atom,
    ACTIONS(116), 2,
      sym_variable,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [876] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(118), 1,
      anon_sym_DOT,
    ACTIONS(120), 1,
      anon_sym_SEMI,
    STATE(37), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [895] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(123), 1,
      anon_sym_DOT,
    ACTIONS(125), 1,
      anon_sym_SEMI,
    STATE(39), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [914] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(128), 1,
      anon_sym_DASH_GT,
    ACTIONS(130), 1,
      anon_sym_when,
    STATE(77), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [933] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 1,
      anon_sym_DOT,
    ACTIONS(134), 1,
      anon_sym_SEMI,
    STATE(39), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [952] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(137), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [967] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(139), 1,
      anon_sym_COMMA,
    ACTIONS(142), 1,
      anon_sym_RBRACK,
    STATE(41), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [986] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(144), 1,
      anon_sym_COMMA,
    ACTIONS(147), 1,
      anon_sym_RBRACK,
    STATE(41), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1005] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(151), 1,
      anon_sym_COMMA,
    ACTIONS(149), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1022] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(153), 1,
      anon_sym_COMMA,
    ACTIONS(156), 1,
      anon_sym_RBRACK,
    STATE(42), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1041] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(48), 1,
      sym_function_clause_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1060] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1074] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1088] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1102] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      anon_sym_RPAREN,
    ACTIONS(164), 1,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1118] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(166), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1132] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(142), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1146] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(168), 1,
      anon_sym_DOT,
    ACTIONS(170), 1,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1162] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(76), 1,
      sym_atom,
    STATE(51), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1178] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(172), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1192] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(174), 1,
      anon_sym_LBRACK,
    STATE(63), 1,
      sym_export_attribute_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1208] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(176), 1,
      anon_sym_module,
    ACTIONS(178), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1224] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(180), 1,
      anon_sym_COMMA,
    ACTIONS(182), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1240] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(184), 1,
      anon_sym_LPAREN,
    STATE(38), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1256] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(186), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1269] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(188), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1282] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(190), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1295] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(192), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1308] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(194), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1321] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(196), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1334] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(182), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1347] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(198), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1360] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(200), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1373] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(202), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1386] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(204), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1399] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(206), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1412] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(208), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1425] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(210), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1438] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(212), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1451] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(214), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1464] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1477] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(216), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1490] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(218), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1503] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(220), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1516] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(222), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1529] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(224), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 58,
  [SMALL_STATE(4)] = 113,
  [SMALL_STATE(5)] = 162,
  [SMALL_STATE(6)] = 211,
  [SMALL_STATE(7)] = 260,
  [SMALL_STATE(8)] = 299,
  [SMALL_STATE(9)] = 338,
  [SMALL_STATE(10)] = 364,
  [SMALL_STATE(11)] = 390,
  [SMALL_STATE(12)] = 410,
  [SMALL_STATE(13)] = 438,
  [SMALL_STATE(14)] = 458,
  [SMALL_STATE(15)] = 479,
  [SMALL_STATE(16)] = 498,
  [SMALL_STATE(17)] = 518,
  [SMALL_STATE(18)] = 536,
  [SMALL_STATE(19)] = 556,
  [SMALL_STATE(20)] = 580,
  [SMALL_STATE(21)] = 601,
  [SMALL_STATE(22)] = 618,
  [SMALL_STATE(23)] = 635,
  [SMALL_STATE(24)] = 654,
  [SMALL_STATE(25)] = 679,
  [SMALL_STATE(26)] = 696,
  [SMALL_STATE(27)] = 713,
  [SMALL_STATE(28)] = 730,
  [SMALL_STATE(29)] = 751,
  [SMALL_STATE(30)] = 768,
  [SMALL_STATE(31)] = 786,
  [SMALL_STATE(32)] = 804,
  [SMALL_STATE(33)] = 822,
  [SMALL_STATE(34)] = 840,
  [SMALL_STATE(35)] = 858,
  [SMALL_STATE(36)] = 876,
  [SMALL_STATE(37)] = 895,
  [SMALL_STATE(38)] = 914,
  [SMALL_STATE(39)] = 933,
  [SMALL_STATE(40)] = 952,
  [SMALL_STATE(41)] = 967,
  [SMALL_STATE(42)] = 986,
  [SMALL_STATE(43)] = 1005,
  [SMALL_STATE(44)] = 1022,
  [SMALL_STATE(45)] = 1041,
  [SMALL_STATE(46)] = 1060,
  [SMALL_STATE(47)] = 1074,
  [SMALL_STATE(48)] = 1088,
  [SMALL_STATE(49)] = 1102,
  [SMALL_STATE(50)] = 1118,
  [SMALL_STATE(51)] = 1132,
  [SMALL_STATE(52)] = 1146,
  [SMALL_STATE(53)] = 1162,
  [SMALL_STATE(54)] = 1178,
  [SMALL_STATE(55)] = 1192,
  [SMALL_STATE(56)] = 1208,
  [SMALL_STATE(57)] = 1224,
  [SMALL_STATE(58)] = 1240,
  [SMALL_STATE(59)] = 1256,
  [SMALL_STATE(60)] = 1269,
  [SMALL_STATE(61)] = 1282,
  [SMALL_STATE(62)] = 1295,
  [SMALL_STATE(63)] = 1308,
  [SMALL_STATE(64)] = 1321,
  [SMALL_STATE(65)] = 1334,
  [SMALL_STATE(66)] = 1347,
  [SMALL_STATE(67)] = 1360,
  [SMALL_STATE(68)] = 1373,
  [SMALL_STATE(69)] = 1386,
  [SMALL_STATE(70)] = 1399,
  [SMALL_STATE(71)] = 1412,
  [SMALL_STATE(72)] = 1425,
  [SMALL_STATE(73)] = 1438,
  [SMALL_STATE(74)] = 1451,
  [SMALL_STATE(75)] = 1464,
  [SMALL_STATE(76)] = 1477,
  [SMALL_STATE(77)] = 1490,
  [SMALL_STATE(78)] = 1503,
  [SMALL_STATE(79)] = 1516,
  [SMALL_STATE(80)] = 1529,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [27] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(58),
  [30] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(56),
  [33] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [35] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(33),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_remote_expr, 1),
  [46] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [48] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [50] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [52] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_remote_expr, 3),
  [56] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_call_open, 2),
  [58] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_open, 2),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [62] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 1), SHIFT(4),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [69] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(4),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 2),
  [74] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 3),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 4),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 2),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equal_op_expr_block, 2),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [90] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 2), SHIFT(4),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [99] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(31),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_clause_open, 4),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 4),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [110] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_function_clause_open, 3),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 3),
  [114] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_equal_op_expr_open, 2),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equal_op_expr_open, 2),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 1),
  [120] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 1), SHIFT(45),
  [123] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 2),
  [125] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 2), SHIFT(45),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [132] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2),
  [134] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2), SHIFT_REPEAT(45),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [139] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(53),
  [142] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [144] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2), SHIFT(53),
  [147] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [149] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 1, .production_id = 1),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [153] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1), SHIFT(53),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 2, .production_id = 1),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [166] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_block, 2),
  [168] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 1),
  [170] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [172] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [182] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [188] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 2),
  [200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [202] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [204] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 2),
  [206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [208] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 3),
  [212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [214] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [216] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [218] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [220] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 4),
  [222] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [224] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
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
