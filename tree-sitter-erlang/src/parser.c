#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 76
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 56
#define ALIAS_COUNT 1
#define TOKEN_COUNT 25
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
  sym_integer = 16,
  anon_sym_DQUOTE = 17,
  aux_sym_string_token1 = 18,
  sym__escape_sequence = 19,
  sym_comment = 20,
  sym_line_comment = 21,
  sym__newline = 22,
  sym_multiple_newlines = 23,
  sym__spaces = 24,
  sym_source_file = 25,
  sym_form = 26,
  sym__attribute = 27,
  sym_module_attribute = 28,
  sym_export_attribute = 29,
  sym_export_attribute_block = 30,
  sym_export_attribute_mfas = 31,
  sym_export_attribute_mfa = 32,
  sym__function_or_macro = 33,
  sym_function_clauses_trailing_semicolon = 34,
  sym_function_clauses = 35,
  sym_function_clause_block = 36,
  sym_function_clause_open = 37,
  sym_pat_argument_list = 38,
  sym_clause_guard = 39,
  sym_function_clause_exprs_trailing_comma = 40,
  sym_exprs = 41,
  sym_expr = 42,
  sym_function_call_block = 43,
  sym_function_call_open = 44,
  sym_expr_remote = 45,
  sym_expr_max = 46,
  sym__atomic = 47,
  sym_strings = 48,
  sym_string = 49,
  aux_sym_source_file_repeat1 = 50,
  aux_sym_export_attribute_mfas_repeat1 = 51,
  aux_sym_function_clauses_repeat1 = 52,
  aux_sym_exprs_repeat1 = 53,
  aux_sym_strings_repeat1 = 54,
  aux_sym_string_repeat1 = 55,
  anon_alias_sym_function_clause_exprs = 56,
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
  [sym_expr_remote] = "expr_remote",
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
  [anon_sym_COLON] = anon_sym_COLON,
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
  [sym_expr_remote] = sym_expr_remote,
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
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
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
  [sym_expr_remote] = {
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
      if (lookahead == '\n') ADVANCE(30);
      if (lookahead == '"') ADVANCE(29);
      if (lookahead == '%') ADVANCE(33);
      if (lookahead == '\'') ADVANCE(34);
      if (lookahead == '(') ADVANCE(40);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == ',') ADVANCE(40);
      if (lookahead == '-') ADVANCE(35);
      if (lookahead == '.') ADVANCE(40);
      if (lookahead == '/') ADVANCE(40);
      if (lookahead == ':') ADVANCE(40);
      if (lookahead == ';') ADVANCE(40);
      if (lookahead == '[') ADVANCE(40);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == ']') ADVANCE(40);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(40);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(39);
      if (lookahead != 0) ADVANCE(40);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(30);
      if (lookahead == '"') ADVANCE(29);
      if (lookahead == '%') ADVANCE(33);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(40);
      if (lookahead != 0) ADVANCE(40);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(46);
      if (lookahead == '%') ADVANCE(3);
      if (lookahead == '\'') ADVANCE(4);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(48);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(45);
      if (lookahead != 0) ADVANCE(44);
      END_STATE();
    case 4:
      if (lookahead == '\'') ADVANCE(23);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(21);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
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
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(28);
      END_STATE();
    case 9:
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(4);
      if (lookahead == '\'') ADVANCE(24);
      if (lookahead == '\\') ADVANCE(9);
      END_STATE();
    case 10:
      if (eof) ADVANCE(11);
      if (lookahead == '\n') ADVANCE(46);
      if (lookahead == '"') ADVANCE(29);
      if (lookahead == '%') ADVANCE(3);
      if (lookahead == '\'') ADVANCE(4);
      if (lookahead == '(') ADVANCE(14);
      if (lookahead == ')') ADVANCE(15);
      if (lookahead == ',') ADVANCE(17);
      if (lookahead == '-') ADVANCE(13);
      if (lookahead == '.') ADVANCE(12);
      if (lookahead == '/') ADVANCE(19);
      if (lookahead == ':') ADVANCE(22);
      if (lookahead == ';') ADVANCE(20);
      if (lookahead == '[') ADVANCE(16);
      if (lookahead == ']') ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(48);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
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
      ACCEPT_TOKEN(sym_atom);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_atom);
      if (lookahead == '\'') ADVANCE(23);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '#') ADVANCE(8);
      if (lookahead == '_') ADVANCE(27);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(27);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(30);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '#') ADVANCE(37);
      if (lookahead == '_') ADVANCE(36);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(31);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(31);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\'') ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(34);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '>') ADVANCE(40);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(36);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(38);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(38);
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
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 10},
  [23] = {.lex_state = 10},
  [24] = {.lex_state = 1},
  [25] = {.lex_state = 10},
  [26] = {.lex_state = 10},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 1},
  [29] = {.lex_state = 2},
  [30] = {.lex_state = 10},
  [31] = {.lex_state = 10},
  [32] = {.lex_state = 10},
  [33] = {.lex_state = 10},
  [34] = {.lex_state = 10},
  [35] = {.lex_state = 10},
  [36] = {.lex_state = 10},
  [37] = {.lex_state = 10},
  [38] = {.lex_state = 10},
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
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 10},
  [51] = {.lex_state = 10},
  [52] = {.lex_state = 10},
  [53] = {.lex_state = 10},
  [54] = {.lex_state = 10},
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
  [68] = {.lex_state = 2},
  [69] = {.lex_state = 10},
  [70] = {.lex_state = 10},
  [71] = {.lex_state = 10},
  [72] = {.lex_state = 2},
  [73] = {.lex_state = 10},
  [74] = {.lex_state = 2},
  [75] = {.lex_state = 10},
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
    [sym_source_file] = STATE(56),
    [sym_form] = STATE(4),
    [sym__attribute] = STATE(55),
    [sym_module_attribute] = STATE(55),
    [sym_export_attribute] = STATE(55),
    [sym__function_or_macro] = STATE(55),
    [sym_function_clauses_trailing_semicolon] = STATE(55),
    [sym_function_clauses] = STATE(51),
    [sym_function_clause_block] = STATE(34),
    [sym_function_clause_open] = STATE(3),
    [aux_sym_source_file_repeat1] = STATE(4),
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
  [0] = 14,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(15), 1,
      anon_sym_COMMA,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(12), 1,
      sym_expr_max,
    STATE(18), 1,
      sym_expr,
    STATE(19), 1,
      sym_expr_remote,
    STATE(23), 1,
      sym_function_call_block,
    STATE(44), 1,
      sym_exprs,
    STATE(7), 2,
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
  [48] = 13,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(12), 1,
      sym_expr_max,
    STATE(18), 1,
      sym_expr,
    STATE(19), 1,
      sym_expr_remote,
    STATE(23), 1,
      sym_function_call_block,
    STATE(33), 1,
      sym_exprs,
    STATE(40), 1,
      sym_function_clause_exprs_trailing_comma,
    STATE(7), 2,
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
  [93] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(19), 1,
      ts_builtin_sym_end,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(34), 1,
      sym_function_clause_block,
    STATE(51), 1,
      sym_function_clauses,
    STATE(5), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(55), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [132] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(21), 1,
      ts_builtin_sym_end,
    ACTIONS(23), 1,
      sym_atom,
    ACTIONS(26), 1,
      anon_sym_DASH,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(34), 1,
      sym_function_clause_block,
    STATE(51), 1,
      sym_function_clauses,
    STATE(5), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(55), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [171] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(12), 1,
      sym_expr_max,
    STATE(19), 1,
      sym_expr_remote,
    STATE(22), 1,
      sym_expr,
    STATE(23), 1,
      sym_function_call_block,
    STATE(7), 2,
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
  [210] = 5,
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
    ACTIONS(29), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
  [235] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(33), 1,
      anon_sym_DQUOTE,
    STATE(8), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(31), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
  [260] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(36), 7,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [279] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    STATE(14), 1,
      sym_expr_max,
    STATE(7), 2,
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
  [306] = 3,
    ACTIONS(3), 1,
      sym__newline,
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
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [325] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(42), 1,
      anon_sym_COLON,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(40), 5,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [345] = 3,
    ACTIONS(3), 1,
      sym__newline,
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
      anon_sym_COLON,
  [363] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(46), 5,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [380] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(50), 1,
      anon_sym_COMMA,
    STATE(15), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(48), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [401] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(53), 1,
      sym_atom,
    ACTIONS(55), 1,
      anon_sym_COMMA,
    ACTIONS(57), 1,
      anon_sym_RBRACK,
    STATE(36), 1,
      sym_export_attribute_mfa,
    STATE(48), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [426] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(61), 1,
      anon_sym_COMMA,
    STATE(15), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(59), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [447] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(66), 1,
      anon_sym_COMMA,
    STATE(17), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(64), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [468] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(69), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [487] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(73), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [503] = 4,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
    STATE(28), 1,
      aux_sym_string_repeat1,
    ACTIONS(77), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [521] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(48), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [537] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(69), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [553] = 4,
    ACTIONS(79), 1,
      anon_sym_DQUOTE,
    STATE(24), 1,
      aux_sym_string_repeat1,
    ACTIONS(81), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [571] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(84), 4,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      sym_atom,
      anon_sym_DQUOTE,
  [587] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(86), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [603] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(88), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [619] = 4,
    ACTIONS(90), 1,
      anon_sym_DQUOTE,
    STATE(24), 1,
      aux_sym_string_repeat1,
    ACTIONS(92), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [637] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(94), 1,
      anon_sym_DASH_GT,
    ACTIONS(96), 1,
      anon_sym_when,
    STATE(72), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [656] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(98), 1,
      anon_sym_COMMA,
    ACTIONS(101), 1,
      anon_sym_RBRACK,
    STATE(30), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [675] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(3), 1,
      sym_function_clause_open,
    STATE(45), 1,
      sym_function_clause_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [694] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(103), 1,
      anon_sym_DOT,
    ACTIONS(105), 1,
      anon_sym_SEMI,
    STATE(38), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [713] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(110), 1,
      anon_sym_COMMA,
    ACTIONS(108), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [730] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(112), 1,
      anon_sym_DOT,
    ACTIONS(114), 1,
      anon_sym_SEMI,
    STATE(32), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [749] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(117), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [764] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(119), 1,
      anon_sym_COMMA,
    ACTIONS(122), 1,
      anon_sym_RBRACK,
    STATE(37), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [783] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(124), 1,
      anon_sym_COMMA,
    ACTIONS(127), 1,
      anon_sym_RBRACK,
    STATE(30), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [802] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(129), 1,
      anon_sym_DOT,
    ACTIONS(131), 1,
      anon_sym_SEMI,
    STATE(38), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [821] = 3,
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
  [835] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(134), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [849] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(53), 1,
      sym_atom,
    STATE(39), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [865] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(136), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [879] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(138), 1,
      anon_sym_module,
    ACTIONS(140), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [895] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(142), 1,
      anon_sym_RPAREN,
    ACTIONS(144), 1,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [911] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(129), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [925] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [939] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 2,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [953] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(150), 1,
      anon_sym_COMMA,
    ACTIONS(152), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [969] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [983] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 2,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [997] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 1,
      anon_sym_DOT,
    ACTIONS(160), 1,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1013] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      anon_sym_LBRACK,
    STATE(58), 1,
      sym_export_attribute_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1029] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(164), 1,
      anon_sym_LPAREN,
    STATE(29), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1045] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(166), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1058] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(168), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1071] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(170), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1084] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(172), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1097] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(174), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1110] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(176), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1123] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(152), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1136] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(178), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1149] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(180), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1162] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(182), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1175] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(184), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1188] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(186), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1201] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(188), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1214] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(190), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1227] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(192), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1240] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(194), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1253] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(142), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1266] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(196), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1279] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(198), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1292] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(200), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1305] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(202), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1318] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(204), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 48,
  [SMALL_STATE(4)] = 93,
  [SMALL_STATE(5)] = 132,
  [SMALL_STATE(6)] = 171,
  [SMALL_STATE(7)] = 210,
  [SMALL_STATE(8)] = 235,
  [SMALL_STATE(9)] = 260,
  [SMALL_STATE(10)] = 279,
  [SMALL_STATE(11)] = 306,
  [SMALL_STATE(12)] = 325,
  [SMALL_STATE(13)] = 345,
  [SMALL_STATE(14)] = 363,
  [SMALL_STATE(15)] = 380,
  [SMALL_STATE(16)] = 401,
  [SMALL_STATE(17)] = 426,
  [SMALL_STATE(18)] = 447,
  [SMALL_STATE(19)] = 468,
  [SMALL_STATE(20)] = 487,
  [SMALL_STATE(21)] = 503,
  [SMALL_STATE(22)] = 521,
  [SMALL_STATE(23)] = 537,
  [SMALL_STATE(24)] = 553,
  [SMALL_STATE(25)] = 571,
  [SMALL_STATE(26)] = 587,
  [SMALL_STATE(27)] = 603,
  [SMALL_STATE(28)] = 619,
  [SMALL_STATE(29)] = 637,
  [SMALL_STATE(30)] = 656,
  [SMALL_STATE(31)] = 675,
  [SMALL_STATE(32)] = 694,
  [SMALL_STATE(33)] = 713,
  [SMALL_STATE(34)] = 730,
  [SMALL_STATE(35)] = 749,
  [SMALL_STATE(36)] = 764,
  [SMALL_STATE(37)] = 783,
  [SMALL_STATE(38)] = 802,
  [SMALL_STATE(39)] = 821,
  [SMALL_STATE(40)] = 835,
  [SMALL_STATE(41)] = 849,
  [SMALL_STATE(42)] = 865,
  [SMALL_STATE(43)] = 879,
  [SMALL_STATE(44)] = 895,
  [SMALL_STATE(45)] = 911,
  [SMALL_STATE(46)] = 925,
  [SMALL_STATE(47)] = 939,
  [SMALL_STATE(48)] = 953,
  [SMALL_STATE(49)] = 969,
  [SMALL_STATE(50)] = 983,
  [SMALL_STATE(51)] = 997,
  [SMALL_STATE(52)] = 1013,
  [SMALL_STATE(53)] = 1029,
  [SMALL_STATE(54)] = 1045,
  [SMALL_STATE(55)] = 1058,
  [SMALL_STATE(56)] = 1071,
  [SMALL_STATE(57)] = 1084,
  [SMALL_STATE(58)] = 1097,
  [SMALL_STATE(59)] = 1110,
  [SMALL_STATE(60)] = 1123,
  [SMALL_STATE(61)] = 1136,
  [SMALL_STATE(62)] = 1149,
  [SMALL_STATE(63)] = 1162,
  [SMALL_STATE(64)] = 1175,
  [SMALL_STATE(65)] = 1188,
  [SMALL_STATE(66)] = 1201,
  [SMALL_STATE(67)] = 1214,
  [SMALL_STATE(68)] = 1227,
  [SMALL_STATE(69)] = 1240,
  [SMALL_STATE(70)] = 1253,
  [SMALL_STATE(71)] = 1266,
  [SMALL_STATE(72)] = 1279,
  [SMALL_STATE(73)] = 1292,
  [SMALL_STATE(74)] = 1305,
  [SMALL_STATE(75)] = 1318,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [23] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(53),
  [26] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(43),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [33] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(21),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 1),
  [42] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 3),
  [48] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [50] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(6),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [61] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 2), SHIFT(6),
  [64] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [66] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 1), SHIFT(6),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [71] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 3),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [77] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [81] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(24),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_open, 2),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 4),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 2),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [98] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(41),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 2),
  [105] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 2), SHIFT(31),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 1, .production_id = 1),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 1),
  [114] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 1), SHIFT(31),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [119] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1), SHIFT(41),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [124] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2), SHIFT(41),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [129] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2),
  [131] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2), SHIFT_REPEAT(31),
  [134] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_block, 2),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 2, .production_id = 1),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [148] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 3),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [154] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_open, 4),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 1),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [170] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [176] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [178] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 2),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 2),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [190] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 3),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [196] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [198] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [200] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 4),
  [202] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
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
