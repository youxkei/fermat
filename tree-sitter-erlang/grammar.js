// workaround for the issue https://github.com/tree-sitter/tree-sitter/issues/761
prec.nonassoc = prec.left;

function repeatSep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

function repeatSep(rule, separator) {
  return optional(repeatSep1(rule, separator));
}

function repeatComma1(rule) {
  return repeatSep1(rule, ",");
}

function repeatComma(rule) {
  return repeatSep(rule, ",");
}

const PREC = {
  catch_: 1,
  equal_exclam: 100,
  double_colon: 150,
  orelse: 150,
  andalso: 160,
  bar: 170,
  comp_op: 200,
  doubledot: 200,
  list_op: 300,
  add_op: 400,
  mult_op: 500,
  prefix_op: 600,
  numbersign: 700,
  colon: 800,
};

module.exports = grammar({
  name: "erlang",

  word: ($) => $.atom,

  extras: ($) => [
    $.comment,
    $.line_comment,
    $.multiple_newlines,
    $._newline,
    $._spaces,
  ],

  conflicts: ($) => [
    [$.export_attribute_mfas],
    [$.exprs],
    [$.function_clauses],
  ],

  rules: {
    source_file: ($) => repeat1($.form),

    form: ($) => seq(choice($._attribute, $._function_or_macro), "."),

    _attribute: ($) =>
      choice(
        $.module_attribute,
        $.export_attribute
        //$.import_attribute,
        //$.record_attribute,
        //$.file_attribute,
        //$.type_attribute,
        //$.opaque_attribute,
        //$.spec_attribute,
        //$.callback_attribute,
        //$.other_attribute
      ),

    module_attribute: ($) => seq("-", "module", "(", $.atom, ")"),

    export_attribute: ($) =>
      seq("-", "export", "(", $.export_attribute_block, ")"),

    export_attribute_block: ($) =>
      seq("[", optional($.export_attribute_mfas), optional(","), "]"),

    export_attribute_mfas: ($) => repeatComma1($.export_attribute_mfa),

    export_attribute_mfa: ($) => seq($.atom, "/", $.integer),

    _function_or_macro: ($) =>
      choice($.function_clauses_trailing_semicolon /*, $.macro*/),

    function_clauses_trailing_semicolon: ($) =>
      seq($.function_clauses, optional(";")),

    function_clauses: ($) => repeatSep1($.function_clause_block, ";"),

    function_clause_block: ($) =>
      seq($.function_clause_open, $.function_clause_exprs_trailing_comma),

    function_clause_open: ($) =>
      seq($.atom, $.pat_argument_list, optional($.clause_guard), "->"),

    pat_argument_list: ($) => seq("(", /*repeatComma($.pat_expr),*/ ")"),

    clause_guard: ($) => seq("when" /*, $.guard*/),

    function_clause_exprs_trailing_comma: ($) =>
      seq(alias($.exprs, "function_clause_exprs"), optional(",")),

    exprs: ($) => repeatComma1($.catch_expr),

    catch_expr: ($) => seq(optional($.catch_op), $.equal_exclam_expr),

    equal_exclam_expr: ($) =>
      seq(
        $.orelse_expr,
        // Do not use repeat to avoid that '=' and '!' is mixed up.
        // While this leads choice nesting, it is acceptable because
        // '=' and '!' are unlikely to be nested.
        optional(seq(choice($.equal_op, $.exclam_op), $.equal_exclam_expr))
      ),

    orelse_expr: ($) => seq($.andalso_expr),

    andalso_expr: ($) => seq($.comp_expr),

    comp_expr: ($) => seq($.list_expr),

    list_expr: ($) => seq($.add_expr),

    add_expr: ($) => seq($.mult_expr, repeat(seq($.add_op, $.mult_expr))),

    mult_expr: ($) => seq($.prefix_expr),

    prefix_expr: ($) => seq($._other_expr),

    _other_expr: ($) =>
      choice(
        $.remote_expr,
        $.function_call_block,
        // $.map_expr,
        // $.record_expr,
        $._primary_expr
      ),

    remote_expr: ($) => seq($._primary_expr, ":", $._primary_expr),

    function_call_block: ($) =>
      seq($.function_call_open, optional($.exprs), optional(","), ")"),

    function_call_open: ($) => seq($.remote_expr, "("),

    //argument_list: ($) => seq("(", optional($.exprs), ")"),

    //pat_expr: ($) =>
    //  choice(
    //    $.binary_pat_expr,
    //    $.unary_pat_expr,
    //    $.map_pat_expr,
    //    $.record_pat_expr,
    //    $.pat_expr_max
    //  ),

    _primary_expr: ($) => choice($.variable, $._atomic),

    catch_op: (_) => seq("catch"),

    equal_op: (_) => choice("="),

    exclam_op: (_) => choice("!"),

    add_op: (_) => choice("+", "-", "bor", "bxor", "bsl", "bsr", "or", "xor"),

    variable: (_) => token(/[A-Z][a-zA-Z0-9_]*/),

    _atomic: ($) => choice($.atom, $.integer, $.strings),

    atom: (_) => token(choice(/'(\\.|[^'])*'/, /[a-z][a-zA-Z0-9_@]*/)),

    integer: (_) => token(choice(/-?\d[\d_]*/, /-?\d+#[a-fA-F\d][a-fA-F\d_]*/)),

    strings: ($) => repeat1($.string),

    string: ($) =>
      seq(
        '"',
        repeat(choice(token.immediate(prec(1, /[^"\\]+/)), $._escape_sequence)),
        '"'
      ),

    _escape_sequence: ($) =>
      token.immediate(
        seq("\\", choice(/[^xuU]/, /\d{1,3}/, /x[0-9a-fA-F]{1,}/))
      ),

    comment: (_) => token(/%[^%].*/),
    line_comment: (_) => token(/%%.*/),
    _newline: (_) => token(/\n/),
    multiple_newlines: (_) => token(/\n\n+/),
    _spaces: (_) => token(/[ \t\r]/),
  },
});
// vim: ts=2 sw=2 sts=2
