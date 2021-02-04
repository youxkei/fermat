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
      seq("-", "export", "(", $.export_attribute_mfas, ")"),

    export_attribute_mfas: ($) =>
      seq("[", repeatComma1($.export_attribute_mfa), optional(","), "]"),

    export_attribute_mfa: ($) => seq($.atom, "/", $.integer),

    _function_or_macro: ($) => choice($.function_clauses /*, $.macro*/),

    function_clauses: ($) =>
      seq(repeatSep1($.function_clause, ";"), optional(";")),

    function_clause: ($) =>
      seq($.function_clause_open, repeatComma1($._expr), optional(",")),

    function_clause_open: ($) =>
      seq($.atom, $.pat_argument_list, optional($.clause_guard), "->"),

    pat_argument_list: ($) => seq("(", /*repeatComma($.pat_expr),*/ ")"),

    clause_guard: ($) => seq("when" /*, $.guard*/),

    _expr: ($) =>
      choice(
        $.binary_expr,
        $.unarry_expr,
        $.remote_expr,
        $.function_call,
        $._primary_expr
      ),

    binary_expr: ($) =>
      choice(
        prec.right(
          PREC.equal_exclam,
          seq($._expr, choice($.equal_op, $.exclam_op), $._expr)
        ),
        prec.right(PREC.list_op, seq($._expr, $.list_op, $._expr)),
        prec.right(PREC.add_op, seq($._expr, $.add_op, $._expr)),
        prec.right(PREC.mult_op, seq($._expr, $.mult_op, $._expr))
      ),

    equal_op: (_) => choice("="),

    exclam_op: (_) => choice("!"),

    add_op: (_) => choice("+", "-", "bor", "bxor", "bsl", "bsr", "or", "xor"),

    mult_op: (_) => choice("/", "*", "div", "rem", "band", "and"),

    list_op: (_) => choice("++", "--"),

    unarry_expr: ($) => choice(prec(PREC.catch_, seq($.catch_op, $._expr))),

    catch_op: (_) => choice("catch"),

    remote_expr: ($) => seq($._primary_expr, ":", $._primary_expr),

    function_call: ($) =>
      seq($.function_call_open, repeatComma($._expr), optional(","), ")"),

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

    _primary_expr: ($) => choice($.list, $.variable, $._atomic),

    list: ($) => seq("[", repeatSep1($._expr, ","), optional(","), "]"),

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

    _escape_sequence: (_) =>
      token.immediate(
        seq("\\", choice(/[^xuU]/, /\d{1,3}/, /x[0-9a-fA-F]{1,}/))
      ),

    comment: (_) => /%[^%].*/,
    line_comment: (_) => /%%.*/,
    _newline: (_) => "\n",
    multiple_newlines: (_) => /\n\n+/,
    _spaces: (_) => /[ \t\r]/,
  },
});
// vim: ts=2 sw=2 sts=2
