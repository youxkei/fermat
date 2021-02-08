// workaround for the issue https://github.com/tree-sitter/tree-sitter/issues/761
prec.nonassoc = prec.right;

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

  conflicts: ($) => [[$.fun_ref_expr, $.fun_open]],

  rules: {
    source_file: ($) => repeat1($.form),

    form: ($) => seq(choice($._attribute, $._function_or_macro), "."),

    _attribute: ($) =>
      choice(
        $.module_attribute,
        $.export_attribute,
        //$.import_attribute,
        //$.record_attribute,
        //$.file_attribute,
        //$.type_attribute,
        //$.opaque_attribute,
        //$.spec_attribute,
        //$.callback_attribute,
        $.other_attribute
      ),

    module_attribute: ($) => seq("-", "module", "(", $._atom_or_macro, ")"),

    export_attribute: ($) =>
      seq("-", "export", "(", $.export_attribute_mfas, ")"),

    export_attribute_mfas: ($) =>
      seq("[", repeatComma1($.export_attribute_mfa), optional(","), "]"),

    export_attribute_mfa: ($) =>
      seq($._atom_or_macro, "/", choice($.integer, $.macro)),

    other_attribute: ($) =>
      seq($.other_attribute_open, repeatComma1($._expr), optional(","), ")"),

    other_attribute_open: ($) => seq("-", $._atom_or_macro, "("),

    _function_or_macro: ($) => choice($.function_clauses /*, $.macro*/),

    function_clauses: ($) =>
      seq(repeatSep1($.function_clause, ";"), optional(";")),

    function_clause: ($) =>
      seq($.function_clause_open, repeatComma1($._expr), optional(",")),

    function_clause_open: ($) =>
      seq(
        $._atom_or_macro,
        $.pat_argument_list,
        optional($.clause_guard),
        "->"
      ),

    pat_argument_list: ($) =>
      seq("(", repeatComma($._pat_expr), optional(","), ")"),

    clause_guard: ($) => seq("when", $.guard),

    guard: ($) => seq(repeatSep1($.exprs, ";"), optional(";")),

    exprs: ($) => seq(repeatComma1($._expr), optional(",")),

    _expr: ($) =>
      choice(
        $.binary_expr,
        $.unary_expr,
        $.map_expr,
        $.function_call,
        $.record_index_expr,
        $.record_expr,
        $.remote_expr,
        $._primary_expr
      ),

    binary_expr: ($) =>
      choice(
        prec.right(
          PREC.equal_exclam,
          seq($._expr, choice($.equal_op, $.exclam_op), $._expr)
        ),
        prec.right(PREC.orelse, seq($._expr, $.orelse_op, $._expr)),
        prec.right(PREC.andalso, seq($._expr, $.andalso_op, $._expr)),
        prec.nonassoc(PREC.comp_op, seq($._expr, $.comp_op, $._expr)),
        prec.right(PREC.list_op, seq($._expr, $.list_op, $._expr)),
        prec.right(PREC.add_op, seq($._expr, $.add_op, $._expr)),
        prec.right(PREC.mult_op, seq($._expr, $.mult_op, $._expr))
      ),

    unary_expr: ($) =>
      choice(
        prec(PREC.catch_, seq("catch", $._expr)),
        prec(PREC.prefix_op, seq($.prefix_op, $._expr))
      ),

    map_expr: ($) =>
      seq($.map_expr_open, repeatComma($.map_expr_field), optional(","), "}"),

    map_expr_open: ($) =>
      seq(optional(choice($._primary_expr, $.map_expr)), "#", "{"),

    map_expr_field: ($) => seq($._expr, $.map_op, $._expr),

    function_call: ($) =>
      seq($.function_call_open, repeatComma($._expr), optional(","), ")"),

    function_call_open: ($) => seq(choice($.remote_expr, $.macro), "("),

    record_index_expr: ($) =>
      seq(
        optional(choice($._primary_expr, $.record_expr)),
        "#",
        $._atom_or_macro,
        ".",
        $._atom_or_macro
      ),

    record_expr: ($) =>
      seq(
        $.record_expr_open,
        repeatComma($.record_expr_field),
        optional(","),
        "}"
      ),

    record_expr_open: ($) =>
      seq(
        optional(choice($._primary_expr, $.record_expr)),
        "#",
        $._atom_or_macro,
        "{"
      ),

    record_expr_field: ($) =>
      seq(choice($.variable, $._atom_or_macro), $.equal_op, $._expr),

    remote_expr: ($) => seq($._primary_expr, ":", $._primary_expr),

    _primary_expr: ($) =>
      choice(
        $.variable,
        $._atomic,
        $.list,
        $.binary,
        $.tuple,
        $.paren_expr,
        $.begin_end_expr,
        //$.if_expr
        //$.case_expr
        //$.receive_expr
        $.fun_ref_expr,
        $.fun_expr,
        $.fun_expr_with_head
        //$.try_expr
      ),

    list: ($) =>
      choice(seq("[", repeatSep($._expr, ","), optional(","), $.list_close)),

    list_close: ($) => seq(optional(seq("|", $.list_tail)), "]"),

    list_tail: ($) => choice($._expr),

    binary: ($) =>
      seq("<<", repeatComma($.binary_element), optional(","), ">>"),

    binary_element: ($) =>
      seq(
        optional($.prefix_op),
        $._primary_expr,
        optional(seq(":", $._primary_expr)),
        optional(
          seq(
            "/",
            repeatSep1(
              seq(
                $._atom_or_macro,
                optional(seq(":", choice($.integer, $.macro)))
              ),
              "-"
            )
          )
        )
      ),

    tuple: ($) => seq("{", repeatComma($._expr), optional(","), "}"),

    paren_expr: ($) => seq("(", $._expr, ")"),

    begin_end_expr: ($) =>
      seq($.begin_open, repeatComma1($._expr), optional(","), $.end_close),

    fun_ref_expr: ($) => seq("fun", $.fun_ref_expr_tail),

    fun_ref_expr_tail: ($) =>
      choice(
        seq($._atom_or_macro, "/", choice($.integer, $.macro)),
        seq(
          choice($.atom, $.variable, $.macro),
          ":",
          choice($.atom, $.variable, $.macro),
          "/",
          choice($.integer, $.variable, $.macro)
        )
      ),

    fun_expr: ($) =>
      seq(
        $.fun_open,
        repeatSep1($.fun_clause, ";"),
        optional(";"),
        $.end_close
      ),

    fun_clause: ($) =>
      seq($.fun_clause_open, repeatComma1($._expr), optional(",")),

    fun_clause_open: ($) => seq($.pat_argument_list, "->"),

    fun_expr_with_head: ($) =>
      seq(
        $.fun_open,
        repeatSep1($.fun_clause_with_head, ";"),
        optional(";"),
        $.end_close
      ),

    fun_clause_with_head: ($) =>
      seq($.fun_clause_with_head_open, repeatComma1($._expr), optional(",")),

    fun_clause_with_head_open: ($) =>
      seq($.variable, $.pat_argument_list, "->"),

    _pat_expr: ($) =>
      choice(
        $.pat_binary_expr,
        $.pat_unary_expr,
        $.pat_map_expr,
        $.pat_record_index_expr,
        $.pat_record_expr,
        $._pat_primary_expr
      ),

    pat_binary_expr: ($) =>
      choice(
        prec.right(
          PREC.equal_exclam,
          seq($._pat_expr, $.equal_op, $._pat_expr)
        ),
        prec.right(PREC.orelse, seq($._pat_expr, $.orelse_op, $._pat_expr)),
        prec.right(PREC.andalso, seq($._pat_expr, $.andalso_op, $._pat_expr)),
        prec.nonassoc(PREC.comp_op, seq($._pat_expr, $.comp_op, $._pat_expr)),
        prec.right(PREC.list_op, seq($._pat_expr, $.list_op, $._pat_expr)),
        prec.right(PREC.add_op, seq($._pat_expr, $.add_op, $._pat_expr)),
        prec.right(PREC.mult_op, seq($._pat_expr, $.mult_op, $._pat_expr))
      ),

    pat_unary_expr: ($) => seq($.prefix_op, $._pat_expr),

    pat_map_expr: ($) =>
      seq(
        $.pat_map_expr_open,
        repeatComma($.pat_map_expr_field),
        optional(","),
        "}"
      ),

    pat_map_expr_open: ($) =>
      seq(optional(choice($._pat_primary_expr, $.pat_map_expr)), "#", "{"),

    pat_map_expr_field: ($) => seq($._pat_expr, $.map_op, $._pat_expr),

    pat_record_index_expr: ($) =>
      seq("#", $._atom_or_macro, ".", $._atom_or_macro),

    pat_record_expr: ($) =>
      seq(
        $.pat_record_expr_open,
        repeatComma($.pat_record_expr_field),
        optional(","),
        "}"
      ),

    pat_record_expr_open: ($) => seq("#", $._atom_or_macro, "{"),

    pat_record_expr_field: ($) =>
      seq(choice($.variable, $._atom_or_macro), $.equal_op, $._pat_expr),

    _pat_primary_expr: ($) =>
      choice(
        $.variable,
        $._atomic,
        $.pat_list,
        $.pat_binary,
        $.pat_tuple,
        $.pat_paren_expr
      ),

    pat_list: ($) =>
      choice(
        seq("[", "]"),
        seq("[", repeatSep1($._pat_expr, ","), optional(","), $.pat_list_close)
      ),

    pat_list_close: ($) => seq(optional(seq("|", $.pat_list_tail)), "]"),

    pat_list_tail: ($) => choice($._pat_expr),

    pat_binary: ($) =>
      seq("<<", repeatComma($.pat_binary_element), optional(","), ">>"),

    pat_binary_element: ($) =>
      seq(
        optional($.prefix_op),
        $._pat_primary_expr,
        optional(seq(":", $._pat_primary_expr)),
        optional(
          seq(
            "/",
            repeatSep1(
              seq(
                $._atom_or_macro,
                optional(seq(":", choice($.integer, $.macro)))
              ),
              "-"
            )
          )
        )
      ),

    pat_tuple: ($) => seq("{", repeatComma($._pat_expr), optional(","), "}"),

    pat_paren_expr: ($) => seq("(", $._pat_expr, ")"),

    _atomic: ($) => choice($._atom_or_macro, $.integer, $.strings),

    prefix_op: (_) => token(choice("+", "-", "bnot", "not")),

    equal_op: (_) => "=",

    exclam_op: (_) => "!",

    orelse_op: (_) => "orelse",

    andalso_op: (_) => "andalso",

    comp_op: (_) =>
      token(choice("==", "/=", "=<", "<", ">=", ">", "=:=", "=/=")),

    list_op: (_) => token(choice("++", "--")),

    add_op: (_) =>
      token(choice("+", "-", "bor", "bxor", "bsl", "bsr", "or", "xor")),

    mult_op: (_) => token(choice("/", "*", "div", "rem", "band", "and")),

    map_op: (_) => token(choice("=>", ":=")),

    begin_open: ($) => "begin",

    end_close: ($) => "end",

    fun_open: ($) => "fun",

    variable: (_) => /[A-Z][a-zA-Z0-9_]*/,

    _atom_or_macro: ($) => choice($.atom, $.macro),

    atom: (_) => token(choice(/'(\\.|[^'])*'/, /[a-z][a-zA-Z0-9_@]*/)),

    macro: (_) => token(/\?[a-zA-Z_][a-zA-Z0-9_@]*/),

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
