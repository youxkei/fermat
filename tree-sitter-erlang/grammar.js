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
      seq("-", "export", "(", "[", repeatComma($.export_mfa), "]", ")"),

    export_mfa: ($) => seq($.atom, "/", $.integer),

    _function_or_macro: ($) => choice($["function"] /*, $.macro*/),

    ["function"]: ($) => repeatSep1($.function_clause, ";"),

    function_clause: ($) =>
      seq($.atom, $.pat_argument_list, optional($.clause_guard), "->", $.exprs),

    pat_argument_list: ($) => seq("(", /*repeatComma($.pat_expr),*/ ")"),

    clause_guard: ($) => seq("when" /*, $.guard*/),

    exprs: ($) => repeatComma1($.expr),

    expr: ($) =>
      choice(
        //$.binary_expr,
        //$.unary_expr,
        //$.map_expr,
        $.function_call,
        //$.record_expr,
        $.expr_remote
      ),

    function_call: ($) => seq($.expr_remote, $.argument_list),

    expr_remote: ($) =>
      choice(
        $.expr_max,
        prec.nonassoc(PREC.colon, seq($.expr_max, ":", $.expr_max))
      ),

    expr_max: ($) => choice($._atomic),

    argument_list: ($) => seq("(", optional($.exprs), ")"),

    //pat_expr: ($) =>
    //  choice(
    //    $.binary_pat_expr,
    //    $.unary_pat_expr,
    //    $.map_pat_expr,
    //    $.record_pat_expr,
    //    $.pat_expr_max
    //  ),

    _atomic: ($) => choice($.atom, $.strings),

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