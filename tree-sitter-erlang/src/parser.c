#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 66
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 53
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
  sym_export_attribute_block = 30,
  sym_export_attribute_mfas = 31,
  sym_export_attribute_mfa = 32,
  sym__function_or_macro = 33,
  sym_function_clauses = 34,
  sym_function_clause = 35,
  sym_pat_argument_list = 36,
  sym_clause_guard = 37,
  sym_exprs = 38,
  sym_expr = 39,
  sym_function_call_block = 40,
  sym_function_call_open = 41,
  sym_expr_remote = 42,
  sym_expr_max = 43,
  sym__atomic = 44,
  sym_strings = 45,
  sym_string = 46,
  aux_sym_source_file_repeat1 = 47,
  aux_sym_export_attribute_mfas_repeat1 = 48,
  aux_sym_function_clauses_repeat1 = 49,
  aux_sym_exprs_repeat1 = 50,
  aux_sym_strings_repeat1 = 51,
  aux_sym_string_repeat1 = 52,
  anon_alias_sym_function_clause_exprs = 53,
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
  [sym_export_attribute_block] = "export_attribute_block",
  [sym_export_attribute_mfas] = "export_attribute_mfas",
  [sym_export_attribute_mfa] = "export_attribute_mfa",
  [sym__function_or_macro] = "_function_or_macro",
  [sym_function_clauses] = "function_clauses",
  [sym_function_clause] = "function_clause",
  [sym_pat_argument_list] = "pat_argument_list",
  [sym_clause_guard] = "clause_guard",
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
  [sym_export_attribute_block] = sym_export_attribute_block,
  [sym_export_attribute_mfas] = sym_export_attribute_mfas,
  [sym_export_attribute_mfa] = sym_export_attribute_mfa,
  [sym__function_or_macro] = sym__function_or_macro,
  [sym_function_clauses] = sym_function_clauses,
  [sym_function_clause] = sym_function_clause,
  [sym_pat_argument_list] = sym_pat_argument_list,
  [sym_clause_guard] = sym_clause_guard,
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

static TSSymbol ts_alias_sequences[3][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [1] = {
    [3] = anon_alias_sym_function_clause_exprs,
  },
  [2] = {
    [4] = anon_alias_sym_function_clause_exprs,
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
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 10},
  [23] = {.lex_state = 10},
  [24] = {.lex_state = 10},
  [25] = {.lex_state = 10},
  [26] = {.lex_state = 10},
  [27] = {.lex_state = 1},
  [28] = {.lex_state = 10},
  [29] = {.lex_state = 10},
  [30] = {.lex_state = 10},
  [31] = {.lex_state = 10},
  [32] = {.lex_state = 10},
  [33] = {.lex_state = 10},
  [34] = {.lex_state = 2},
  [35] = {.lex_state = 10},
  [36] = {.lex_state = 10},
  [37] = {.lex_state = 10},
  [38] = {.lex_state = 10},
  [39] = {.lex_state = 10},
  [40] = {.lex_state = 10},
  [41] = {.lex_state = 10},
  [42] = {.lex_state = 10},
  [43] = {.lex_state = 10},
  [44] = {.lex_state = 2},
  [45] = {.lex_state = 10},
  [46] = {.lex_state = 10},
  [47] = {.lex_state = 10},
  [48] = {.lex_state = 10},
  [49] = {.lex_state = 10},
  [50] = {.lex_state = 2},
  [51] = {.lex_state = 10},
  [52] = {.lex_state = 2},
  [53] = {.lex_state = 2},
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
    [sym_source_file] = STATE(61),
    [sym_form] = STATE(7),
    [sym__attribute] = STATE(60),
    [sym_module_attribute] = STATE(60),
    [sym_export_attribute] = STATE(60),
    [sym__function_or_macro] = STATE(60),
    [sym_function_clauses] = STATE(60),
    [sym_function_clause] = STATE(31),
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
    STATE(13), 1,
      sym_expr_max,
    STATE(18), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
    STATE(48), 1,
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
  [45] = 12,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(13), 1,
      sym_expr_max,
    STATE(18), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
    STATE(39), 1,
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
  [87] = 12,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(13), 1,
      sym_expr_max,
    STATE(18), 1,
      sym_expr_remote,
    STATE(19), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
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
  [129] = 11,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(11), 1,
      sym_atom,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    STATE(2), 1,
      sym_function_call_open,
    STATE(13), 1,
      sym_expr_max,
    STATE(18), 1,
      sym_expr_remote,
    STATE(23), 1,
      sym_expr,
    STATE(26), 1,
      sym_function_call_block,
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
  [168] = 8,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(17), 1,
      ts_builtin_sym_end,
    ACTIONS(19), 1,
      sym_atom,
    ACTIONS(22), 1,
      anon_sym_DASH,
    STATE(31), 1,
      sym_function_clause,
    STATE(6), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(60), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses,
  [201] = 8,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(5), 1,
      sym_atom,
    ACTIONS(7), 1,
      anon_sym_DASH,
    ACTIONS(25), 1,
      ts_builtin_sym_end,
    STATE(31), 1,
      sym_function_clause,
    STATE(6), 2,
      sym_form,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    STATE(60), 5,
      sym__attribute,
      sym_module_attribute,
      sym_export_attribute,
      sym__function_or_macro,
      sym_function_clauses,
  [234] = 5,
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
  [259] = 5,
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
  [284] = 3,
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
  [303] = 7,
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
    STATE(14), 2,
      sym__atomic,
      sym_strings,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [330] = 3,
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
  [349] = 4,
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
  [369] = 3,
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
  [387] = 3,
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
  [404] = 5,
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
  [425] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(53), 1,
      anon_sym_COMMA,
    STATE(16), 1,
      aux_sym_exprs_repeat1,
    ACTIONS(51), 3,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [446] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(57), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(55), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [465] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(53), 1,
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
  [486] = 4,
    ACTIONS(61), 1,
      anon_sym_DQUOTE,
    STATE(21), 1,
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
  [504] = 4,
    ACTIONS(65), 1,
      anon_sym_DQUOTE,
    STATE(21), 1,
      aux_sym_string_repeat1,
    ACTIONS(67), 2,
      aux_sym_string_token1,
      sym__escape_sequence,
    ACTIONS(3), 5,
      sym_comment,
      sym_line_comment,
      sym__newline,
      sym_multiple_newlines,
      sym__spaces,
  [522] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(70), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [538] = 3,
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
  [554] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(72), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [570] = 6,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(74), 1,
      sym_atom,
    ACTIONS(76), 1,
      anon_sym_RBRACK,
    STATE(29), 1,
      sym_export_attribute_mfa,
    STATE(62), 1,
      sym_export_attribute_mfas,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [592] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
    ACTIONS(55), 4,
      anon_sym_DOT,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [608] = 4,
    ACTIONS(78), 1,
      anon_sym_DQUOTE,
    STATE(20), 1,
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
  [626] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(82), 3,
      anon_sym_RPAREN,
      sym_atom,
      anon_sym_DQUOTE,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [641] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(84), 1,
      anon_sym_RBRACK,
    ACTIONS(86), 1,
      anon_sym_COMMA,
    STATE(32), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [660] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(88), 1,
      anon_sym_RBRACK,
    ACTIONS(90), 1,
      anon_sym_COMMA,
    STATE(30), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [679] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_SEMI,
    STATE(36), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [698] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(86), 1,
      anon_sym_COMMA,
    ACTIONS(97), 1,
      anon_sym_RBRACK,
    STATE(30), 1,
      aux_sym_export_attribute_mfas_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [717] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(99), 1,
      anon_sym_DOT,
    ACTIONS(101), 1,
      anon_sym_SEMI,
    STATE(33), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [736] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(104), 1,
      anon_sym_DASH_GT,
    ACTIONS(106), 1,
      anon_sym_when,
    STATE(50), 1,
      sym_clause_guard,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [755] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(108), 3,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [770] = 5,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(95), 1,
      anon_sym_SEMI,
    ACTIONS(110), 1,
      anon_sym_DOT,
    STATE(33), 1,
      aux_sym_function_clauses_repeat1,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [789] = 4,
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
  [805] = 3,
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
  [819] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(114), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [833] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(88), 2,
      anon_sym_RBRACK,
      anon_sym_COMMA,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [847] = 3,
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
  [861] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    STATE(34), 1,
      sym_pat_argument_list,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [877] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(120), 1,
      anon_sym_module,
    ACTIONS(122), 1,
      anon_sym_export,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [893] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(124), 2,
      anon_sym_DASH_GT,
      anon_sym_when,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [907] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(126), 1,
      anon_sym_LBRACK,
    STATE(49), 1,
      sym_export_attribute_block,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [923] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(99), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [937] = 4,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(74), 1,
      sym_atom,
    STATE(40), 1,
      sym_export_attribute_mfa,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [953] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [966] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(130), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [979] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(132), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [992] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(134), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1005] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(136), 1,
      anon_sym_DASH_GT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1018] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(138), 1,
      sym_integer,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1031] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(140), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1044] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(142), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1057] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(144), 1,
      sym_atom,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1070] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(146), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1083] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(148), 1,
      anon_sym_LPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1096] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(150), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1109] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(152), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1122] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(154), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1135] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(156), 1,
      anon_sym_RBRACK,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1148] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(158), 1,
      anon_sym_SLASH,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1161] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(160), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
  [1174] = 3,
    ACTIONS(3), 1,
      sym__newline,
    ACTIONS(162), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      sym_comment,
      sym_line_comment,
      sym_multiple_newlines,
      sym__spaces,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 45,
  [SMALL_STATE(4)] = 87,
  [SMALL_STATE(5)] = 129,
  [SMALL_STATE(6)] = 168,
  [SMALL_STATE(7)] = 201,
  [SMALL_STATE(8)] = 234,
  [SMALL_STATE(9)] = 259,
  [SMALL_STATE(10)] = 284,
  [SMALL_STATE(11)] = 303,
  [SMALL_STATE(12)] = 330,
  [SMALL_STATE(13)] = 349,
  [SMALL_STATE(14)] = 369,
  [SMALL_STATE(15)] = 387,
  [SMALL_STATE(16)] = 404,
  [SMALL_STATE(17)] = 425,
  [SMALL_STATE(18)] = 446,
  [SMALL_STATE(19)] = 465,
  [SMALL_STATE(20)] = 486,
  [SMALL_STATE(21)] = 504,
  [SMALL_STATE(22)] = 522,
  [SMALL_STATE(23)] = 538,
  [SMALL_STATE(24)] = 554,
  [SMALL_STATE(25)] = 570,
  [SMALL_STATE(26)] = 592,
  [SMALL_STATE(27)] = 608,
  [SMALL_STATE(28)] = 626,
  [SMALL_STATE(29)] = 641,
  [SMALL_STATE(30)] = 660,
  [SMALL_STATE(31)] = 679,
  [SMALL_STATE(32)] = 698,
  [SMALL_STATE(33)] = 717,
  [SMALL_STATE(34)] = 736,
  [SMALL_STATE(35)] = 755,
  [SMALL_STATE(36)] = 770,
  [SMALL_STATE(37)] = 789,
  [SMALL_STATE(38)] = 805,
  [SMALL_STATE(39)] = 819,
  [SMALL_STATE(40)] = 833,
  [SMALL_STATE(41)] = 847,
  [SMALL_STATE(42)] = 861,
  [SMALL_STATE(43)] = 877,
  [SMALL_STATE(44)] = 893,
  [SMALL_STATE(45)] = 907,
  [SMALL_STATE(46)] = 923,
  [SMALL_STATE(47)] = 937,
  [SMALL_STATE(48)] = 953,
  [SMALL_STATE(49)] = 966,
  [SMALL_STATE(50)] = 979,
  [SMALL_STATE(51)] = 992,
  [SMALL_STATE(52)] = 1005,
  [SMALL_STATE(53)] = 1018,
  [SMALL_STATE(54)] = 1031,
  [SMALL_STATE(55)] = 1044,
  [SMALL_STATE(56)] = 1057,
  [SMALL_STATE(57)] = 1070,
  [SMALL_STATE(58)] = 1083,
  [SMALL_STATE(59)] = 1096,
  [SMALL_STATE(60)] = 1109,
  [SMALL_STATE(61)] = 1122,
  [SMALL_STATE(62)] = 1135,
  [SMALL_STATE(63)] = 1148,
  [SMALL_STATE(64)] = 1161,
  [SMALL_STATE(65)] = 1174,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [19] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(42),
  [22] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(43),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2),
  [29] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_strings_repeat1, 2), SHIFT_REPEAT(27),
  [32] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_strings, 1),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 1),
  [40] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_max, 1),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr_remote, 3),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_exprs_repeat1, 2), SHIFT_REPEAT(5),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 2),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_exprs, 1),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2),
  [67] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(21),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 3),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_block, 2),
  [74] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_call_open, 2),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 1),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2),
  [90] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_export_attribute_mfas_repeat1, 2), SHIFT_REPEAT(47),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 1),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfas, 2),
  [99] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2),
  [101] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_clauses_repeat1, 2), SHIFT_REPEAT(37),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_form, 2),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clauses, 2),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 5, .production_id = 2),
  [114] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4, .production_id = 1),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_mfa, 3),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [124] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pat_argument_list, 2),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_clause_guard, 1),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [140] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 3),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute, 5),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [154] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_export_attribute_block, 2),
  [162] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 5),
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
