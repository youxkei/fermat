#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 75
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 55
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
  sym_function_clause = 36,
  sym_pat_argument_list = 37,
  sym_clause_guard = 38,
  sym_function_clause_exprs_trailing_comma = 39,
  sym_exprs = 40,
  sym_expr = 41,
  sym_function_call_block = 42,
  sym_function_call_open = 43,
  sym_expr_remote = 44,
  sym_expr_max = 45,
  sym__atomic = 46,
  sym_strings = 47,
  sym_string = 48,
  aux_sym_source_file_repeat1 = 49,
  aux_sym_export_attribute_mfas_repeat1 = 50,
  aux_sym_function_clauses_repeat1 = 51,
  aux_sym_exprs_repeat1 = 52,
  aux_sym_strings_repeat1 = 53,
  aux_sym_string_repeat1 = 54,
  anon_alias_sym_function_clause_exprs = 55,
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
  [sym_function_clause] = "function_clause",
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
  [sym_function_clause] = sym_function_clause,
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
  [sym_function_clause] = {
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
  [19] = {.lex_state = 1},
  [20] = {.lex_state = 10},
  [21] = {.lex_state = 10},
  [22] = {.lex_state = 10},
  [23] = {.lex_state = 1},
  [24] = {.lex_state = 10},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 10},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 10},
  [29] = {.lex_state = 10},
  [30] = {.lex_state = 10},
  [31] = {.lex_state = 10},
  [32] = {.lex_state = 10},
  [33] = {.lex_state = 2},
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
  [48] = {.lex_state = 2},
  [49] = {.lex_state = 10},
  [50] = {.lex_state = 10},
  [51] = {.lex_state = 10},
  [52] = {.lex_state = 10},
  [53] = {.lex_state = 10},
  [54] = {.lex_state = 10},
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
  [72] = {.lex_state = 2},
  [73] = {.lex_state = 2},
  [74] = {.lex_state = 10},
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
    [sym_source_file] = STATE(67),
    [sym_form] = STATE(6),
    [sym__attribute] = STATE(66),
    [sym_module_attribute] = STATE(66),
    [sym_export_attribute] = STATE(66),
    [sym__function_or_macro] = STATE(66),
    [sym_function_clauses_trailing_semicolon] = STATE(66),
    [sym_function_clauses] = STATE(43),
    [sym_function_clause] = STATE(39),
    [aux_sym_source_file_repeat1] = STATE(6),
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
  [0] = 13,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(17), 1,
      sym_expr_remote,
    STATE(26), 1,
      sym_function_call_block,
    STATE(32), 1,
      sym_expr,
    STATE(57), 1,
      sym_exprs,
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
  [45] = 13,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(17), 1,
      sym_expr_remote,
    STATE(22), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
    STATE(34), 1,
      sym_exprs,
    STATE(45), 1,
      sym_function_clause_exprs_trailing_comma,
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
  [90] = 13,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(17), 1,
      sym_expr_remote,
    STATE(22), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
    STATE(34), 1,
      sym_exprs,
    STATE(53), 1,
      sym_function_clause_exprs_trailing_comma,
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
  [135] = 9,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      ts_builtin_sym_end,
    ACTIONS(19), 1,
      sym_atom,
    ACTIONS(22), 1,
      anon_sym_DASH,
    STATE(39), 1,
      sym_function_clause,
    STATE(43), 1,
      sym_function_clauses,
    STATE(5), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(66), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [171] = 9,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(25), 1,
      ts_builtin_sym_end,
    STATE(39), 1,
      sym_function_clause,
    STATE(43), 1,
      sym_function_clauses,
    STATE(5), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(66), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses_trailing_semicolon,
  [207] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(14), 1,
      sym_expr_max,
    STATE(17), 1,
      sym_expr_remote,
    STATE(24), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
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
  [246] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(29), 1,
      anon_sym_DQUOTE,
    STATE(8), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(27), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
  [271] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(8), 2,
      sym_string,
      aux_sym_strings_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(32), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
  [296] = 3,
    ACTIONS(3), 1,
      sym__newline,
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
      anon_sym_COLON,
      anon_sym_DQUOTE,
  [315] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(15), 1,
      sym_expr_max,
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
  [342] = 3,
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
  [361] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(38), 6,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_COLON,
  [379] = 4,
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
  [399] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(44), 5,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [416] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(48), 1,
      anon_sym_COMMA,
    STATE(16), 1,
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
  [437] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(53), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(51), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [456] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(55), 1,
      sym_atom,
    ACTIONS(57), 1,
      anon_sym_COMMA,
    ACTIONS(59), 1,
      anon_sym_RBRACK,
    STATE(29), 1,
      sym_export_attribute_mfa,
    STATE(44), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [481] = 4,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    STATE(19), 1,
      aux_sym_string_repeat1,
    ACTIONS(63), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [499] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(66), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [515] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(70), 1,
      anon_sym_COMMA,
    STATE(16), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(68), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [535] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(75), 1,
      anon_sym_COMMA,
    STATE(21), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(73), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [555] = 4,
    ACTIONS(78), 1,
      anon_sym_DQUOTE,
    STATE(19), 1,
      aux_sym_string_repeat1,
    ACTIONS(80), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [573] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(46), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [589] = 4,
    ACTIONS(82), 1,
      anon_sym_DQUOTE,
    STATE(23), 1,
      aux_sym_string_repeat1,
    ACTIONS(84), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [607] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(51), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [623] = 3,
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
  [639] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(88), 3,
      anon_sym_RPAREN,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [654] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(90), 1,
      anon_sym_COMMA,
    ACTIONS(93), 1,
      anon_sym_RBRACK,
    STATE(38), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [673] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(95), 1,
      anon_sym_DOT,
    ACTIONS(97), 1,
      anon_sym_SEMI,
    STATE(30), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [692] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(68), 1,
      anon_sym_RPAREN,
    ACTIONS(100), 1,
      anon_sym_COMMA,
    STATE(16), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [711] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(73), 1,
      anon_sym_RPAREN,
    ACTIONS(100), 1,
      anon_sym_COMMA,
    STATE(31), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [730] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(102), 1,
      anon_sym_DASH_GT,
    ACTIONS(104), 1,
      anon_sym_when,
    STATE(73), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [749] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(108), 1,
      anon_sym_COMMA,
    ACTIONS(106), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [766] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(110), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [781] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(112), 1,
      anon_sym_COMMA,
    ACTIONS(115), 1,
      anon_sym_RBRACK,
    STATE(36), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [800] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(117), 1,
      anon_sym_DOT,
    ACTIONS(119), 1,
      anon_sym_SEMI,
    STATE(30), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [819] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(122), 1,
      anon_sym_COMMA,
    ACTIONS(125), 1,
      anon_sym_RBRACK,
    STATE(36), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [838] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(127), 1,
      anon_sym_DOT,
    ACTIONS(129), 1,
      anon_sym_SEMI,
    STATE(37), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [857] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 1,
      anon_sym_module,
    ACTIONS(134), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [873] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(136), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [887] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(138), 1,
      anon_sym_LPAREN,
    STATE(33), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [903] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(140), 1,
      anon_sym_DOT,
    ACTIONS(142), 1,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [919] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(144), 1,
      anon_sym_COMMA,
    ACTIONS(146), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [935] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [949] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(95), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [963] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(150), 1,
      anon_sym_LBRACK,
    STATE(74), 1,
      sym_export_attribute_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [979] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(152), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [993] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1007] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(46), 1,
      sym_function_clause,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1023] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(55), 1,
      sym_atom,
    STATE(52), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1039] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(115), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1053] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1067] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1080] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1093] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1106] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(164), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1119] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(166), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1132] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(168), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1145] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(170), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1158] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(172), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1171] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(174), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1184] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(176), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1197] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(178), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1210] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(180), 1,
      anon_sym_DOT,
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
    ACTIONS(184), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1249] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(186), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1262] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(188), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1275] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(190), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1288] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1301] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(192), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1314] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(194), 1,
      anon_sym_DASH_GT,
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
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 45,
  [SMALL_STATE(4)] = 90,
  [SMALL_STATE(5)] = 135,
  [SMALL_STATE(6)] = 171,
  [SMALL_STATE(7)] = 207,
  [SMALL_STATE(8)] = 246,
  [SMALL_STATE(9)] = 271,
  [SMALL_STATE(10)] = 296,
  [SMALL_STATE(11)] = 315,
  [SMALL_STATE(12)] = 342,
  [SMALL_STATE(13)] = 361,
  [SMALL_STATE(14)] = 379,
  [SMALL_STATE(15)] = 399,
  [SMALL_STATE(16)] = 416,
  [SMALL_STATE(17)] = 437,
  [SMALL_STATE(18)] = 456,
  [SMALL_STATE(19)] = 481,
  [SMALL_STATE(20)] = 499,
  [SMALL_STATE(21)] = 515,
  [SMALL_STATE(22)] = 535,
  [SMALL_STATE(23)] = 555,
  [SMALL_STATE(24)] = 573,
  [SMALL_STATE(25)] = 589,
  [SMALL_STATE(26)] = 607,
  [SMALL_STATE(27)] = 623,
  [SMALL_STATE(28)] = 639,
  [SMALL_STATE(29)] = 654,
  [SMALL_STATE(30)] = 673,
  [SMALL_STATE(31)] = 692,
  [SMALL_STATE(32)] = 711,
  [SMALL_STATE(33)] = 730,
  [SMALL_STATE(34)] = 749,
  [SMALL_STATE(35)] = 766,
  [SMALL_STATE(36)] = 781,
  [SMALL_STATE(37)] = 800,
  [SMALL_STATE(38)] = 819,
  [SMALL_STATE(39)] = 838,
  [SMALL_STATE(40)] = 857,
  [SMALL_STATE(41)] = 873,
  [SMALL_STATE(42)] = 887,
  [SMALL_STATE(43)] = 903,
  [SMALL_STATE(44)] = 919,
  [SMALL_STATE(45)] = 935,
  [SMALL_STATE(46)] = 949,
  [SMALL_STATE(47)] = 963,
  [SMALL_STATE(48)] = 979,
  [SMALL_STATE(49)] = 993,
  [SMALL_STATE(50)] = 1007,
  [SMALL_STATE(51)] = 1023,
  [SMALL_STATE(52)] = 1039,
  [SMALL_STATE(53)] = 1053,
  [SMALL_STATE(54)] = 1067,
  [SMALL_STATE(55)] = 1080,
  [SMALL_STATE(56)] = 1093,
  [SMALL_STATE(57)] = 1106,
  [SMALL_STATE(58)] = 1119,
  [SMALL_STATE(59)] = 1132,
  [SMALL_STATE(60)] = 1145,
  [SMALL_STATE(61)] = 1158,
  [SMALL_STATE(62)] = 1171,
  [SMALL_STATE(63)] = 1184,
  [SMALL_STATE(64)] = 1197,
  [SMALL_STATE(65)] = 1210,
  [SMALL_STATE(66)] = 1223,
  [SMALL_STATE(67)] = 1236,
  [SMALL_STATE(68)] = 1249,
  [SMALL_STATE(69)] = 1262,
  [SMALL_STATE(70)] = 1275,
  [SMALL_STATE(71)] = 1288,
  [SMALL_STATE(72)] = 1301,
  [SMALL_STATE(73)] = 1314,
  [SMALL_STATE(74)] = 1327,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [19] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(42),
  [22] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(40),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [29] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(25),
  [32] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 1),
  [42] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 3),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(7),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(19),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 2),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [70] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 2), SHIFT(7),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [75] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_exprs, 1), SHIFT(7),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 3),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_open, 2),
  [90] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1), SHIFT(51),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [95] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2),
  [97] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2), SHIFT_REPEAT(50),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [106] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 1, .production_id = 1),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [112] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(51),
  [115] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 2),
  [119] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 2), SHIFT(50),
  [122] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2), SHIFT(51),
  [125] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 1),
  [129] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_function_clauses, 1), SHIFT(50),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [140] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 1),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [148] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [154] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause_exprs_trailing_comma, 2, .production_id = 1),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 5),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [166] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 3),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [170] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [174] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses_trailing_semicolon, 2),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [180] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [182] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [184] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [186] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [188] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 2),
  [190] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 4),
  [192] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [196] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
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
