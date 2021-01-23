#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 79
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 60
#define ALIAS_COUNT 1
#define TOKEN_COUNT 27
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
  anon_sym_EQ = 15,
  anon_sym_COLON = 16,
  sym_variable = 17,
  sym_integer = 18,
  anon_sym_DQUOTE = 19,
  aux_sym_string_token1 = 20,
  sym__escape_sequence = 21,
  sym_comment = 22,
  sym_line_comment = 23,
  sym__newline = 24,
  sym_multiple_newlines = 25,
  sym__spaces = 26,
  sym_source_file = 27,
  sym_form = 28,
  sym__attribute = 29,
  sym_module_attribute = 30,
  sym_export_attribute = 31,
  sym_export_attribute_block = 32,
  sym_export_attribute_mfas = 33,
  sym_export_attribute_mfa = 34,
  sym__function_or_macro = 35,
  sym_function_clauses_trailing_semicolon = 36,
  sym_function_clauses = 37,
  sym_function_clause_block = 38,
  sym_function_clause_open = 39,
  sym_pat_argument_list = 40,
  sym_clause_guard = 41,
  sym_function_clause_exprs_trailing_comma = 42,
  sym_exprs = 43,
  sym_expr = 44,
  sym_equal_expr_block = 45,
  sym_equal_expr_open = 46,
  sym_function_call_block = 47,
  sym_function_call_open = 48,
  sym_remote_expr = 49,
  sym_expr_max = 50,
  sym__atomic = 51,
  sym_strings = 52,
  sym_string = 53,
  aux_sym_source_file_repeat1 = 54,
  aux_sym_export_attribute_mfas_repeat1 = 55,
  aux_sym_function_clauses_repeat1 = 56,
  aux_sym_exprs_repeat1 = 57,
  aux_sym_strings_repeat1 = 58,
  aux_sym_string_repeat1 = 59,
  anon_alias_sym_function_clause_exprs = 60,
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
  [sym_equal_expr_block] = "equal_expr_block",
  [sym_equal_expr_open] = "equal_expr_open",
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
  [sym_equal_expr_block] = sym_equal_expr_block,
  [sym_equal_expr_open] = sym_equal_expr_open,
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
  [sym_equal_expr_block] = {
    .visible = true,
    .named = true,
  },
  [sym_equal_expr_open] = {
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
      if (lookahead == 'e') ADVANCE(1);
      if (lookahead == 'm') ADVANCE(2);
      if (lookahead == 'w') ADVANCE(3);
      END_STATE();
    case 1:
      if (lookahead == 'x') ADVANCE(4);
      END_STATE();
    case 2:
      if (lookahead == 'o') ADVANCE(5);
      END_STATE();
    case 3:
      if (lookahead == 'h') ADVANCE(6);
      END_STATE();
    case 4:
      if (lookahead == 'p') ADVANCE(7);
      END_STATE();
    case 5:
      if (lookahead == 'd') ADVANCE(8);
      END_STATE();
    case 6:
      if (lookahead == 'e') ADVANCE(9);
      END_STATE();
    case 7:
      if (lookahead == 'o') ADVANCE(10);
      END_STATE();
    case 8:
      if (lookahead == 'u') ADVANCE(11);
      END_STATE();
    case 9:
      if (lookahead == 'n') ADVANCE(12);
      END_STATE();
    case 10:
      if (lookahead == 'r') ADVANCE(13);
      END_STATE();
    case 11:
      if (lookahead == 'l') ADVANCE(14);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_when);
      END_STATE();
    case 13:
      if (lookahead == 't') ADVANCE(15);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(16);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_export);
      END_STATE();
    case 16:
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
  [28] = {.lex_state = 1},
  [29] = {.lex_state = 1},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 10},
  [32] = {.lex_state = 10},
  [33] = {.lex_state = 10},
  [34] = {.lex_state = 10},
  [35] = {.lex_state = 10},
  [36] = {.lex_state = 10},
  [37] = {.lex_state = 10},
  [38] = {.lex_state = 10},
  [39] = {.lex_state = 2},
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
  [54] = {.lex_state = 10},
  [55] = {.lex_state = 10},
  [56] = {.lex_state = 2},
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
  [70] = {.lex_state = 10},
  [71] = {.lex_state = 2},
  [72] = {.lex_state = 10},
  [73] = {.lex_state = 10},
  [74] = {.lex_state = 10},
  [75] = {.lex_state = 10},
  [76] = {.lex_state = 10},
  [77] = {.lex_state = 2},
  [78] = {.lex_state = 2},
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
    [sym_source_file] = STATE(61),
    [sym_form] = STATE(7),
    [sym__attribute] = STATE(59),
    [sym_module_attribute] = STATE(59),
    [sym_export_attribute] = STATE(59),
    [sym__function_or_macro] = STATE(59),
    [sym_function_clauses_trailing_semicolon] = STATE(59),
    [sym_function_clauses] = STATE(52),
    [sym_function_clause_block] = STATE(33),
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
  [0] = 15,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(15), 1,
      anon_sym_COMMA,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(4), 1,
      sym_equal_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr,
    STATE(17), 1,
      sym_remote_expr,
    STATE(47), 1,
      sym_exprs,
    ACTIONS(11), 2,
      sym_variable,
      sym_atom,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(13), 2,
      sym__atomic,
      sym_strings,
    STATE(26), 2,
      sym_equal_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [53] = 14,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(4), 1,
      sym_equal_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr,
    STATE(17), 1,
      sym_remote_expr,
    STATE(36), 1,
      sym_exprs,
    STATE(48), 1,
      sym_function_clause_exprs_trailing_comma,
    ACTIONS(11), 2,
      sym_variable,
      sym_atom,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(13), 2,
      sym__atomic,
      sym_strings,
    STATE(26), 2,
      sym_equal_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [103] = 12,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(4), 1,
      sym_equal_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(17), 1,
      sym_remote_expr,
    STATE(21), 1,
      sym_expr,
    ACTIONS(11), 2,
      sym_variable,
      sym_atom,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(13), 2,
      sym__atomic,
      sym_strings,
    STATE(26), 2,
      sym_equal_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [147] = 12,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(4), 1,
      sym_equal_expr_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(17), 1,
      sym_remote_expr,
    STATE(24), 1,
      sym_expr,
    ACTIONS(11), 2,
      sym_variable,
      sym_atom,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(13), 2,
      sym__atomic,
      sym_strings,
    STATE(26), 2,
      sym_equal_expr_block,
      sym_function_call_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [191] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(19), 1,
      ts_builtin_sym_end,
    ACTIONS(21), 1,
      sym_atom,
    ACTIONS(24), 1,
      anon_sym_DASH,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(33), 1,
      sym_function_clause_block,
    STATE(52), 1,
      sym_function_clauses,
    STATE(6), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(59), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [230] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(27), 1,
      ts_builtin_sym_end,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(33), 1,
      sym_function_clause_block,
    STATE(52), 1,
      sym_function_clauses,
    STATE(6), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(59), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [269] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    STATE(8), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(29), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
  [295] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(8), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(34), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
  [321] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(36), 8,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [341] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(16), 1,
      sym_expr_max,
    ACTIONS(11), 2,
      sym_variable,
      sym_atom,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(13), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [369] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(38), 8,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [389] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(40), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
      anon_sym_COLON,
  [408] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(44), 1,
      anon_sym_COLON,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(42), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [429] = 6,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(48), 1,
      anon_sym_COMMA,
    ACTIONS(51), 1,
      anon_sym_EQ,
    STATE(22), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(46), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [453] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(53), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [471] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(57), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(55), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [491] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(59), 1,
      sym_atom,
    ACTIONS(61), 1,
      anon_sym_COMMA,
    ACTIONS(63), 1,
      anon_sym_RBRACK,
    STATE(37), 1,
      sym_export_attribute_mfa,
    STATE(53), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [516] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(65), 5,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      sym_variable,
      sym_atom,
      anon_sym_DQUOTE,
  [533] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(67), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [550] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(69), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [567] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(73), 1,
      anon_sym_COMMA,
    STATE(25), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(71), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [588] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(76), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [605] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(51), 1,
      anon_sym_EQ,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(78), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [624] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(80), 1,
      anon_sym_COMMA,
    STATE(25), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(78), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [645] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(55), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [662] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(83), 5,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_EQ,
  [679] = 4,
    ACTIONS(85), 1,
      anon_sym_DQUOTE,
    STATE(30), 1,
      aux_sym_string_repeat1,
    ACTIONS(87), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [697] = 4,
    ACTIONS(89), 1,
      anon_sym_DQUOTE,
    STATE(29), 1,
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
  [715] = 4,
    ACTIONS(94), 1,
      anon_sym_DQUOTE,
    STATE(29), 1,
      aux_sym_string_repeat1,
    ACTIONS(96), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [733] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(44), 1,
      sym_function_clause_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [752] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(98), 1,
      anon_sym_COMMA,
    ACTIONS(101), 1,
      anon_sym_RBRACK,
    STATE(32), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [771] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(103), 1,
      anon_sym_DOT,
    ACTIONS(105), 1,
      anon_sym_SEMI,
    STATE(43), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [790] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(108), 3,
      sym_variable,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [805] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(110), 3,
      sym_variable,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [820] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(114), 1,
      anon_sym_COMMA,
    ACTIONS(112), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [837] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(116), 1,
      anon_sym_COMMA,
    ACTIONS(119), 1,
      anon_sym_RBRACK,
    STATE(41), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [856] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(121), 1,
      anon_sym_DOT,
    ACTIONS(123), 1,
      anon_sym_SEMI,
    STATE(38), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [875] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(126), 1,
      anon_sym_DASH_GT,
    ACTIONS(128), 1,
      anon_sym_when,
    STATE(77), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [894] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(130), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [909] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 1,
      anon_sym_COMMA,
    ACTIONS(135), 1,
      anon_sym_RBRACK,
    STATE(32), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [928] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(137), 3,
      sym_variable,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [943] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(139), 1,
      anon_sym_DOT,
    ACTIONS(141), 1,
      anon_sym_SEMI,
    STATE(38), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [962] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(121), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [976] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(144), 1,
      anon_sym_LBRACK,
    STATE(67), 1,
      sym_export_attribute_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [992] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(59), 1,
      sym_atom,
    STATE(55), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1008] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 1,
      anon_sym_RPAREN,
    ACTIONS(148), 1,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1024] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(150), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1038] = 3,
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
  [1052] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 1,
      anon_sym_LPAREN,
    STATE(39), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1068] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 1,
      anon_sym_module,
    ACTIONS(158), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1084] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 1,
      anon_sym_DOT,
    ACTIONS(162), 1,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1100] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(164), 1,
      anon_sym_COMMA,
    ACTIONS(166), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1116] = 3,
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
  [1130] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(101), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1144] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(170), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1158] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(172), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1171] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(174), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1184] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(176), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1197] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(178), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1210] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(180), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1223] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(182), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1236] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(166), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1249] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(184), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1262] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(186), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1275] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(188), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1288] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(190), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1301] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(192), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1314] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(194), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1327] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(196), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1340] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(198), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1353] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(200), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1366] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(202), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1379] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1392] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(204), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1405] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(206), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1418] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(208), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1431] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(210), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 53,
  [SMALL_STATE(4)] = 103,
  [SMALL_STATE(5)] = 147,
  [SMALL_STATE(6)] = 191,
  [SMALL_STATE(7)] = 230,
  [SMALL_STATE(8)] = 269,
  [SMALL_STATE(9)] = 295,
  [SMALL_STATE(10)] = 321,
  [SMALL_STATE(11)] = 341,
  [SMALL_STATE(12)] = 369,
  [SMALL_STATE(13)] = 389,
  [SMALL_STATE(14)] = 408,
  [SMALL_STATE(15)] = 429,
  [SMALL_STATE(16)] = 453,
  [SMALL_STATE(17)] = 471,
  [SMALL_STATE(18)] = 491,
  [SMALL_STATE(19)] = 516,
  [SMALL_STATE(20)] = 533,
  [SMALL_STATE(21)] = 550,
  [SMALL_STATE(22)] = 567,
  [SMALL_STATE(23)] = 588,
  [SMALL_STATE(24)] = 605,
  [SMALL_STATE(25)] = 624,
  [SMALL_STATE(26)] = 645,
  [SMALL_STATE(27)] = 662,
  [SMALL_STATE(28)] = 679,
  [SMALL_STATE(29)] = 697,
  [SMALL_STATE(30)] = 715,
  [SMALL_STATE(31)] = 733,
  [SMALL_STATE(32)] = 752,
  [SMALL_STATE(33)] = 771,
  [SMALL_STATE(34)] = 790,
  [SMALL_STATE(35)] = 805,
  [SMALL_STATE(36)] = 820,
  [SMALL_STATE(37)] = 837,
  [SMALL_STATE(38)] = 856,
  [SMALL_STATE(39)] = 875,
  [SMALL_STATE(40)] = 894,
  [SMALL_STATE(41)] = 909,
  [SMALL_STATE(42)] = 928,
  [SMALL_STATE(43)] = 943,
  [SMALL_STATE(44)] = 962,
  [SMALL_STATE(45)] = 976,
  [SMALL_STATE(46)] = 992,
  [SMALL_STATE(47)] = 1008,
  [SMALL_STATE(48)] = 1024,
  [SMALL_STATE(49)] = 1038,
  [SMALL_STATE(50)] = 1052,
  [SMALL_STATE(51)] = 1068,
  [SMALL_STATE(52)] = 1084,
  [SMALL_STATE(53)] = 1100,
  [SMALL_STATE(54)] = 1116,
  [SMALL_STATE(55)] = 1130,
  [SMALL_STATE(56)] = 1144,
  [SMALL_STATE(57)] = 1158,
  [SMALL_STATE(58)] = 1171,
  [SMALL_STATE(59)] = 1184,
  [SMALL_STATE(60)] = 1197,
  [SMALL_STATE(61)] = 1210,
  [SMALL_STATE(62)] = 1223,
  [SMALL_STATE(63)] = 1236,
  [SMALL_STATE(64)] = 1249,
  [SMALL_STATE(65)] = 1262,
  [SMALL_STATE(66)] = 1275,
  [SMALL_STATE(67)] = 1288,
  [SMALL_STATE(68)] = 1301,
  [SMALL_STATE(69)] = 1314,
  [SMALL_STATE(70)] = 1327,
  [SMALL_STATE(71)] = 1340,
  [SMALL_STATE(72)] = 1353,
  [SMALL_STATE(73)] = 1366,
  [SMALL_STATE(74)] = 1379,
  [SMALL_STATE(75)] = 1392,
  [SMALL_STATE(76)] = 1405,
  [SMALL_STATE(77)] = 1418,
  [SMALL_STATE(78)] = 1431,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [21] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(50),
  [24] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(51),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [31] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(28),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_remote_expr, 1),
  [44] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 1), SHIFT(5),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_remote_expr, 3),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_open, 2),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 2),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equal_expr_block, 2),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 2), SHIFT(5),
  [76] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 4),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [80] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(5),
  [83] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 3),
  [85] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [87] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [89] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [91] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(29),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [98] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(46),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 1),
  [105] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 1), SHIFT(31),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 3),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 4),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 1, .production_id = 1),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [116] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1), SHIFT(46),
  [119] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [121] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2),
  [123] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2), SHIFT_REPEAT(31),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [130] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [132] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2), SHIFT(46),
  [135] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equal_expr_open, 2),
  [139] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 2),
  [141] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 2), SHIFT(31),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_block, 2),
  [152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 2, .production_id = 1),
  [154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 1),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [168] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [180] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [184] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 2),
  [186] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [192] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 3),
  [198] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [202] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [204] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 2),
  [206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 4),
  [208] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
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
