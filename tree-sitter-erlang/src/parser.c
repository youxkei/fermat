#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 66
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 53
#define ALIAS_COUNT 0
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
  anon_sym_RBRACK = 9,
  anon_sym_COMMA = 10,
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
  sym_export_attribute_mfa_list = 30,
  sym_export_attribute_mfas = 31,
  sym_export_attribute_mfa = 32,
  sym__function_or_macro = 33,
  sym_function = 34,
  sym_function_clause = 35,
  sym_pat_argument_list = 36,
  sym_clause_guard = 37,
  sym_exprs = 38,
  sym_expr = 39,
  sym_function_call = 40,
  sym_expr_remote = 41,
  sym_expr_max = 42,
  sym_argument_list = 43,
  sym__atomic = 44,
  sym_strings = 45,
  sym_string = 46,
  aux_sym_source_file_repeat1 = 47,
  aux_sym_export_attribute_mfas_repeat1 = 48,
  aux_sym_function_repeat1 = 49,
  aux_sym_exprs_repeat1 = 50,
  aux_sym_strings_repeat1 = 51,
  aux_sym_string_repeat1 = 52,
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
  [anon_sym_RBRACK] = "]",
  [anon_sym_COMMA] = ",",
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
  [sym_export_attribute_mfa_list] = "export_attribute_mfa_list",
  [sym_export_attribute_mfas] = "export_attribute_mfas",
  [sym_export_attribute_mfa] = "export_attribute_mfa",
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
  [aux_sym_export_attribute_mfas_repeat1] = "export_attribute_mfas_repeat1",
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
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_COMMA] = anon_sym_COMMA,
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
  [sym_export_attribute_mfa_list] = sym_export_attribute_mfa_list,
  [sym_export_attribute_mfas] = sym_export_attribute_mfas,
  [sym_export_attribute_mfa] = sym_export_attribute_mfa,
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
  [aux_sym_export_attribute_mfas_repeat1] = aux_sym_export_attribute_mfas_repeat1,
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
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
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
  [sym_export_attribute_mfa_list] = {
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
  [aux_sym_export_attribute_mfas_repeat1] = {
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
      if (lookahead == ',') ADVANCE(18);
      if (lookahead == '-') ADVANCE(13);
      if (lookahead == '.') ADVANCE(12);
      if (lookahead == '/') ADVANCE(19);
      if (lookahead == ':') ADVANCE(22);
      if (lookahead == ';') ADVANCE(20);
      if (lookahead == '[') ADVANCE(16);
      if (lookahead == ']') ADVANCE(17);
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
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_COMMA);
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
  [21] = {.lex_state = 10},
  [22] = {.lex_state = 1},
  [23] = {.lex_state = 10},
  [24] = {.lex_state = 1},
  [25] = {.lex_state = 10},
  [26] = {.lex_state = 10},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 1},
  [29] = {.lex_state = 10},
  [30] = {.lex_state = 10},
  [31] = {.lex_state = 10},
  [32] = {.lex_state = 10},
  [33] = {.lex_state = 10},
  [34] = {.lex_state = 10},
  [35] = {.lex_state = 2},
  [36] = {.lex_state = 10},
  [37] = {.lex_state = 10},
  [38] = {.lex_state = 10},
  [39] = {.lex_state = 10},
  [40] = {.lex_state = 10},
  [41] = {.lex_state = 10},
  [42] = {.lex_state = 10},
  [43] = {.lex_state = 2},
  [44] = {.lex_state = 10},
  [45] = {.lex_state = 10},
  [46] = {.lex_state = 10},
  [47] = {.lex_state = 10},
  [48] = {.lex_state = 10},
  [49] = {.lex_state = 10},
  [50] = {.lex_state = 10},
  [51] = {.lex_state = 2},
  [52] = {.lex_state = 10},
  [53] = {.lex_state = 10},
  [54] = {.lex_state = 10},
  [55] = {.lex_state = 10},
  [56] = {.lex_state = 10},
  [57] = {.lex_state = 10},
  [58] = {.lex_state = 2},
  [59] = {.lex_state = 2},
  [60] = {.lex_state = 10},
  [61] = {.lex_state = 10},
  [62] = {.lex_state = 10},
  [63] = {.lex_state = 10},
  [64] = {.lex_state = 10},
  [65] = {.lex_state = 10},
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
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
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
    [sym_source_file] = STATE(50),
    [sym_form] = STATE(4),
    [sym__attribute] = STATE(49),
    [sym_module_attribute] = STATE(49),
    [sym_export_attribute] = STATE(49),
    [sym__function_or_macro] = STATE(49),
    [sym_function] = STATE(49),
    [sym_function_clause] = STATE(33),
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
  [0] = 12,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(13), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(60), 1,
      sym_exprs,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(14), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [42] = 8,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      ts_builtin_sym_end,
    ACTIONS(19), 1,
      sym_atom,
    ACTIONS(22), 1,
      anon_sym_DASH,
    STATE(33), 1,
      sym_function_clause,
    STATE(3), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(49), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function,
  [75] = 8,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(25), 1,
      ts_builtin_sym_end,
    STATE(33), 1,
      sym_function_clause,
    STATE(3), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(49), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function,
  [108] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(13), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(38), 1,
      sym_exprs,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(14), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [147] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(13), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(46), 1,
      sym_exprs,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(14), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [186] = 10,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(13), 1,
      sym_expr_max,
    STATE(15), 1,
      sym_expr_remote,
    STATE(21), 1,
      sym_expr,
    STATE(27), 1,
      sym_function_call,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(14), 2,
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
  [247] = 5,
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
    STATE(18), 1,
      sym_expr_max,
    STATE(9), 2,
      sym_string,
      aux_sym_strings_repeat1,
    STATE(14), 2,
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
  [337] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(40), 1,
      anon_sym_COLON,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(38), 5,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [357] = 3,
    ACTIONS(3), 1,
      sym__newline,
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
      anon_sym_COLON,
  [375] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(46), 1,
      anon_sym_LPAREN,
    STATE(25), 1,
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
  [397] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(50), 1,
      anon_sym_COMMA,
    STATE(16), 1,
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
  [418] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(55), 1,
      anon_sym_COMMA,
    STATE(16), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(53), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [439] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(57), 5,
      anon_sym_DOT,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [456] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(55), 1,
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
  [493] = 3,
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
  [509] = 4,
    ACTIONS(63), 1,
      anon_sym_DQUOTE,
    STATE(22), 1,
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
  [527] = 6,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(68), 1,
      sym_atom,
    ACTIONS(70), 1,
      anon_sym_RBRACK,
    STATE(32), 1,
      sym_export_attribute_mfa,
    STATE(53), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [549] = 4,
    ACTIONS(72), 1,
      anon_sym_DQUOTE,
    STATE(28), 1,
      aux_sym_string_repeat1,
    ACTIONS(74), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [567] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(76), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [583] = 3,
    ACTIONS(3), 1,
      sym__newline,
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
  [599] = 3,
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
  [615] = 4,
    ACTIONS(80), 1,
      anon_sym_DQUOTE,
    STATE(22), 1,
      aux_sym_string_repeat1,
    ACTIONS(82), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [633] = 5,
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
  [652] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(89), 1,
      anon_sym_RBRACK,
    ACTIONS(91), 1,
      anon_sym_COMMA,
    STATE(34), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [671] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(93), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [686] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(91), 1,
      anon_sym_COMMA,
    ACTIONS(95), 1,
      anon_sym_RBRACK,
    STATE(30), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [705] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(97), 1,
      anon_sym_DOT,
    ACTIONS(99), 1,
      anon_sym_SEMI,
    STATE(36), 1,
      aux_sym_function_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [724] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(101), 1,
      anon_sym_RBRACK,
    ACTIONS(103), 1,
      anon_sym_COMMA,
    STATE(34), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [743] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(106), 1,
      anon_sym_DASH_GT,
    ACTIONS(108), 1,
      anon_sym_when,
    STATE(58), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [762] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(99), 1,
      anon_sym_SEMI,
    ACTIONS(110), 1,
      anon_sym_DOT,
    STATE(29), 1,
      aux_sym_function_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [781] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(68), 1,
      sym_atom,
    STATE(41), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [797] = 3,
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
  [811] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(114), 1,
      anon_sym_LBRACK,
    STATE(54), 1,
      sym_export_attribute_mfa_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [827] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    STATE(44), 1,
      sym_function_clause,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [843] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(101), 2,
      anon_sym_RBRACK,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [857] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(116), 2,
      anon_sym_RBRACK,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [871] = 3,
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
  [885] = 3,
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
  [899] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(120), 1,
      anon_sym_LPAREN,
    STATE(35), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [915] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(122), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [929] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(124), 1,
      anon_sym_module,
    ACTIONS(126), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [945] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [958] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(130), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [971] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [984] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(134), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [997] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(136), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1010] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(138), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1023] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(140), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1036] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(142), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1049] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(144), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1062] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1075] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1088] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(150), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1101] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(152), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1114] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1127] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1140] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1153] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1166] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 42,
  [SMALL_STATE(4)] = 75,
  [SMALL_STATE(5)] = 108,
  [SMALL_STATE(6)] = 147,
  [SMALL_STATE(7)] = 186,
  [SMALL_STATE(8)] = 222,
  [SMALL_STATE(9)] = 247,
  [SMALL_STATE(10)] = 272,
  [SMALL_STATE(11)] = 291,
  [SMALL_STATE(12)] = 318,
  [SMALL_STATE(13)] = 337,
  [SMALL_STATE(14)] = 357,
  [SMALL_STATE(15)] = 375,
  [SMALL_STATE(16)] = 397,
  [SMALL_STATE(17)] = 418,
  [SMALL_STATE(18)] = 439,
  [SMALL_STATE(19)] = 456,
  [SMALL_STATE(20)] = 477,
  [SMALL_STATE(21)] = 493,
  [SMALL_STATE(22)] = 509,
  [SMALL_STATE(23)] = 527,
  [SMALL_STATE(24)] = 549,
  [SMALL_STATE(25)] = 567,
  [SMALL_STATE(26)] = 583,
  [SMALL_STATE(27)] = 599,
  [SMALL_STATE(28)] = 615,
  [SMALL_STATE(29)] = 633,
  [SMALL_STATE(30)] = 652,
  [SMALL_STATE(31)] = 671,
  [SMALL_STATE(32)] = 686,
  [SMALL_STATE(33)] = 705,
  [SMALL_STATE(34)] = 724,
  [SMALL_STATE(35)] = 743,
  [SMALL_STATE(36)] = 762,
  [SMALL_STATE(37)] = 781,
  [SMALL_STATE(38)] = 797,
  [SMALL_STATE(39)] = 811,
  [SMALL_STATE(40)] = 827,
  [SMALL_STATE(41)] = 843,
  [SMALL_STATE(42)] = 857,
  [SMALL_STATE(43)] = 871,
  [SMALL_STATE(44)] = 885,
  [SMALL_STATE(45)] = 899,
  [SMALL_STATE(46)] = 915,
  [SMALL_STATE(47)] = 929,
  [SMALL_STATE(48)] = 945,
  [SMALL_STATE(49)] = 958,
  [SMALL_STATE(50)] = 971,
  [SMALL_STATE(51)] = 984,
  [SMALL_STATE(52)] = 997,
  [SMALL_STATE(53)] = 1010,
  [SMALL_STATE(54)] = 1023,
  [SMALL_STATE(55)] = 1036,
  [SMALL_STATE(56)] = 1049,
  [SMALL_STATE(57)] = 1062,
  [SMALL_STATE(58)] = 1075,
  [SMALL_STATE(59)] = 1088,
  [SMALL_STATE(60)] = 1101,
  [SMALL_STATE(61)] = 1114,
  [SMALL_STATE(62)] = 1127,
  [SMALL_STATE(63)] = 1140,
  [SMALL_STATE(64)] = 1153,
  [SMALL_STATE(65)] = 1166,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [19] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(45),
  [22] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(47),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [29] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(24),
  [32] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 1),
  [40] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [46] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [48] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [50] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(7),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 3),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 2),
  [63] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [65] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(22),
  [68] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [70] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [72] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [74] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [76] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call, 2),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 3),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [86] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(40),
  [89] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [95] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 1),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [103] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(37),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 5),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [128] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa_list, 3),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [132] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [136] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa_list, 2),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
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
