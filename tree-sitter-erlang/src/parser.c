#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 67
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 51
#define ALIAS_COUNT 0
#define TOKEN_COUNT 25
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 8

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
  sym_export_mfa = 30,
  sym__function_or_macro = 31,
  sym_function = 32,
  sym_function_clause = 33,
  sym_pat_argument_list = 34,
  sym_clause_guard = 35,
  sym_exprs = 36,
  sym_expr = 37,
  sym_function_call = 38,
  sym_expr_remote = 39,
  sym_expr_max = 40,
  sym_argument_list = 41,
  sym__atomic = 42,
  sym_strings = 43,
  sym_string = 44,
  aux_sym_source_file_repeat1 = 45,
  aux_sym_export_attribute_repeat1 = 46,
  aux_sym_function_repeat1 = 47,
  aux_sym_exprs_repeat1 = 48,
  aux_sym_strings_repeat1 = 49,
  aux_sym_string_repeat1 = 50,
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
  [sym_export_mfa] = "export_mfa",
  [sym__function_or_macro] = "_function_or_macro",
  [sym_function] = "function",
  [sym_function_clause] = "function_clause",
  [sym_pat_argument_list] = "pat_argument_list",
  [sym_clause_guard] = "clause_guard",
  [sym_exprs] = "exprs",
  [sym_expr] = "expr",
  [sym_function_call] = "function_call",
  [sym_expr_remote] = "expr_remote",
  [sym_expr_max] = "expr_max",
  [sym_argument_list] = "argument_list",
  [sym__atomic] = "_atomic",
  [sym_strings] = "strings",
  [sym_string] = "string",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_export_attribute_repeat1] = "export_attribute_repeat1",
  [aux_sym_function_repeat1] = "function_repeat1",
  [aux_sym_exprs_repeat1] = "exprs_repeat1",
  [aux_sym_strings_repeat1] = "strings_repeat1",
  [aux_sym_string_repeat1] = "string_repeat1",
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
  [sym_export_mfa] = sym_export_mfa,
  [sym__function_or_macro] = sym__function_or_macro,
  [sym_function] = sym_function,
  [sym_function_clause] = sym_function_clause,
  [sym_pat_argument_list] = sym_pat_argument_list,
  [sym_clause_guard] = sym_clause_guard,
  [sym_exprs] = sym_exprs,
  [sym_expr] = sym_expr,
  [sym_function_call] = sym_function_call,
  [sym_expr_remote] = sym_expr_remote,
  [sym_expr_max] = sym_expr_max,
  [sym_argument_list] = sym_argument_list,
  [sym__atomic] = sym__atomic,
  [sym_strings] = sym_strings,
  [sym_string] = sym_string,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_export_attribute_repeat1] = aux_sym_export_attribute_repeat1,
  [aux_sym_function_repeat1] = aux_sym_function_repeat1,
  [aux_sym_exprs_repeat1] = aux_sym_exprs_repeat1,
  [aux_sym_strings_repeat1] = aux_sym_strings_repeat1,
  [aux_sym_string_repeat1] = aux_sym_string_repeat1,
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
  [sym_export_mfa] = {
    .visible = true,
    .named = true,
  },
  [sym__function_or_macro] = {
    .visible = false,
    .named = true,
  },
  [sym_function] = {
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
  [sym_exprs] = {
    .visible = true,
    .named = true,
  },
  [sym_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_function_call] = {
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
  [sym_argument_list] = {
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
  [aux_sym_export_attribute_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_function_repeat1] = {
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
};

static TSSymbol ts_alias_sequences[1][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static uint16_t ts_non_terminal_alias_map[] = {
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
  [24] = {.lex_state = 10},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 1},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 2},
  [29] = {.lex_state = 10},
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
  [40] = {.lex_state = 2},
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
  [56] = {.lex_state = 2},
  [57] = {.lex_state = 2},
  [58] = {.lex_state = 10},
  [59] = {.lex_state = 10},
  [60] = {.lex_state = 10},
  [61] = {.lex_state = 10},
  [62] = {.lex_state = 10},
  [63] = {.lex_state = 10},
  [64] = {.lex_state = 10},
  [65] = {.lex_state = 10},
  [66] = {.lex_state = 10},
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
    [sym_source_file] = STATE(54),
    [sym_form] = STATE(5),
    [sym__attribute] = STATE(65),
    [sym_module_attribute] = STATE(65),
    [sym_export_attribute] = STATE(65),
    [sym__function_or_macro] = STATE(65),
    [sym_function] = STATE(65),
    [sym_function_clause] = STATE(34),
    [aux_sym_source_file_repeat1] = STATE(5),
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
  [0] = 12,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(14), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(58), 1,
      sym_exprs,
    STATE(8), 2,
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
  [42] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(14), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(45), 1,
      sym_exprs,
    STATE(8), 2,
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
  [81] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(14), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(37), 1,
      sym_exprs,
    STATE(8), 2,
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
  [120] = 8,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(17), 1,
      ts_builtin_sym_end,
    STATE(34), 1,
      sym_function_clause,
    STATE(6), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(65), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function,
  [153] = 8,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(19), 1,
      ts_builtin_sym_end,
    ACTIONS(21), 1,
      sym_atom,
    ACTIONS(24), 1,
      anon_sym_DASH,
    STATE(34), 1,
      sym_function_clause,
    STATE(6), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(65), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function,
  [186] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(14), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(23), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(8), 2,
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
  [222] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(9), 2,
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
  [247] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(31), 1,
      anon_sym_DQUOTE,
    STATE(9), 2,
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
  [272] = 3,
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
  [291] = 7,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(16), 1,
      sym_expr_max,
    STATE(8), 2,
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
  [318] = 3,
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
  [337] = 3,
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
  [355] = 4,
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
  [375] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(46), 1,
      anon_sym_LPAREN,
    STATE(24), 1,
      sym_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(44), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [397] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(48), 5,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [414] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(52), 1,
      anon_sym_COMMA,
    STATE(18), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(50), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [435] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(56), 1,
      anon_sym_COMMA,
    STATE(18), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(54), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [456] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(52), 1,
      anon_sym_COMMA,
    STATE(17), 1,
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
  [477] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(61), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [493] = 4,
    ACTIONS(63), 1,
      anon_sym_DQUOTE,
    STATE(25), 1,
      aux_sym_string_repeat1,
    ACTIONS(65), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [511] = 3,
    ACTIONS(3), 1,
      sym__newline,
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
  [527] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(54), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [543] = 3,
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
  [559] = 4,
    ACTIONS(71), 1,
      anon_sym_DQUOTE,
    STATE(25), 1,
      aux_sym_string_repeat1,
    ACTIONS(73), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [577] = 4,
    ACTIONS(76), 1,
      anon_sym_DQUOTE,
    STATE(21), 1,
      aux_sym_string_repeat1,
    ACTIONS(78), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [595] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(44), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [611] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(80), 1,
      anon_sym_DASH_GT,
    ACTIONS(82), 1,
      anon_sym_when,
    STATE(56), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [630] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_SEMI,
    STATE(29), 1,
      aux_sym_function_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [649] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(89), 1,
      sym_atom,
    ACTIONS(91), 1,
      anon_sym_RBRACK,
    STATE(31), 1,
      sym_export_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [668] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(93), 1,
      anon_sym_COMMA,
    ACTIONS(95), 1,
      anon_sym_RBRACK,
    STATE(36), 1,
      aux_sym_export_attribute_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [687] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(97), 1,
      anon_sym_DOT,
    ACTIONS(99), 1,
      anon_sym_SEMI,
    STATE(29), 1,
      aux_sym_function_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [706] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(101), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [721] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(99), 1,
      anon_sym_SEMI,
    ACTIONS(103), 1,
      anon_sym_DOT,
    STATE(32), 1,
      aux_sym_function_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [740] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(105), 1,
      anon_sym_COMMA,
    ACTIONS(108), 1,
      anon_sym_RBRACK,
    STATE(35), 1,
      aux_sym_export_attribute_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [759] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(93), 1,
      anon_sym_COMMA,
    ACTIONS(110), 1,
      anon_sym_RBRACK,
    STATE(35), 1,
      aux_sym_export_attribute_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [778] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(112), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [792] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(114), 1,
      anon_sym_module,
    ACTIONS(116), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [808] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(84), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [822] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(118), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [836] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(39), 1,
      sym_function_clause,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [852] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(120), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [866] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(108), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [880] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(122), 1,
      anon_sym_LPAREN,
    STATE(28), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [896] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(124), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [910] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(89), 1,
      sym_atom,
    STATE(43), 1,
      sym_export_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [926] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(126), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [939] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(128), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [952] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(130), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [965] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [978] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(134), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [991] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(136), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1004] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(138), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1017] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(140), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1030] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(142), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1043] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(144), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1056] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1069] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1082] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(150), 1,
      anon_sym_LBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1095] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(152), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1108] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1121] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1134] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1147] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1160] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1173] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(164), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 42,
  [SMALL_STATE(4)] = 81,
  [SMALL_STATE(5)] = 120,
  [SMALL_STATE(6)] = 153,
  [SMALL_STATE(7)] = 186,
  [SMALL_STATE(8)] = 222,
  [SMALL_STATE(9)] = 247,
  [SMALL_STATE(10)] = 272,
  [SMALL_STATE(11)] = 291,
  [SMALL_STATE(12)] = 318,
  [SMALL_STATE(13)] = 337,
  [SMALL_STATE(14)] = 355,
  [SMALL_STATE(15)] = 375,
  [SMALL_STATE(16)] = 397,
  [SMALL_STATE(17)] = 414,
  [SMALL_STATE(18)] = 435,
  [SMALL_STATE(19)] = 456,
  [SMALL_STATE(20)] = 477,
  [SMALL_STATE(21)] = 493,
  [SMALL_STATE(22)] = 511,
  [SMALL_STATE(23)] = 527,
  [SMALL_STATE(24)] = 543,
  [SMALL_STATE(25)] = 559,
  [SMALL_STATE(26)] = 577,
  [SMALL_STATE(27)] = 595,
  [SMALL_STATE(28)] = 611,
  [SMALL_STATE(29)] = 630,
  [SMALL_STATE(30)] = 649,
  [SMALL_STATE(31)] = 668,
  [SMALL_STATE(32)] = 687,
  [SMALL_STATE(33)] = 706,
  [SMALL_STATE(34)] = 721,
  [SMALL_STATE(35)] = 740,
  [SMALL_STATE(36)] = 759,
  [SMALL_STATE(37)] = 778,
  [SMALL_STATE(38)] = 792,
  [SMALL_STATE(39)] = 808,
  [SMALL_STATE(40)] = 822,
  [SMALL_STATE(41)] = 836,
  [SMALL_STATE(42)] = 852,
  [SMALL_STATE(43)] = 866,
  [SMALL_STATE(44)] = 880,
  [SMALL_STATE(45)] = 896,
  [SMALL_STATE(46)] = 910,
  [SMALL_STATE(47)] = 926,
  [SMALL_STATE(48)] = 939,
  [SMALL_STATE(49)] = 952,
  [SMALL_STATE(50)] = 965,
  [SMALL_STATE(51)] = 978,
  [SMALL_STATE(52)] = 991,
  [SMALL_STATE(53)] = 1004,
  [SMALL_STATE(54)] = 1017,
  [SMALL_STATE(55)] = 1030,
  [SMALL_STATE(56)] = 1043,
  [SMALL_STATE(57)] = 1056,
  [SMALL_STATE(58)] = 1069,
  [SMALL_STATE(59)] = 1082,
  [SMALL_STATE(60)] = 1095,
  [SMALL_STATE(61)] = 1108,
  [SMALL_STATE(62)] = 1121,
  [SMALL_STATE(63)] = 1134,
  [SMALL_STATE(64)] = 1147,
  [SMALL_STATE(65)] = 1160,
  [SMALL_STATE(66)] = 1173,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [21] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(44),
  [24] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(38),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [31] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(26),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 1),
  [42] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [46] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [48] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 3),
  [50] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [52] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [56] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(7),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 3),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 2),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call, 2),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(25),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [86] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(41),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 1),
  [105] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_repeat1, 2), SHIFT_REPEAT(46),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_repeat1, 2),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_mfa, 3),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [124] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 5),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [128] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 6),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [136] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [138] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [140] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 7),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [164] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 8),
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
