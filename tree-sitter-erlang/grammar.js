// workaround for the issue https://github.com/tree-sitter/tree-sitter/issues/761
prec.nonassoc = prec.right;

function repeatSep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)), optional(separator));
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

function repeatSemicolon1(rule) {
  return repeatSep1(rule, ";");
}

function repeatSemicolon(rule) {
  return repeatSep(rule, ";");
}

const PREC = {
  catch: 1,
  equal_exclam: 100,
  orelse: 150,
  andalso: 160,
  union: 170,
  comp: 200,
  range: 200,
  list: 300,
  add: 400,
  mult: 500,
  prefix: 600,
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
    /*
     * "try ok, • catch"
     * - "try ok, catch {throw, T} -> T end": "catch" from try_expr
     * - "try ok, catch 42 after 42 end": "catch" from catch expression
     */
    [$.try_expr_try],

    /*
     * "try ok of ok, • catch"
     * - "try ok of ok -> ok, catch {throw, T} -> T end": "catch" from try_expr
     * - "try ok of ok -> ok, catch 42 after 57 end": "catch" from catch expression
     */
    [$.match_clause],
  ],

  rules: {
    source_file: ($) => repeat1($.form),

    form: ($) => seq(choice($._attribute, $.function_clauses), "."),

    _attribute: ($) =>
      choice(
        $.module_attribute,
        $.export_attribute,
        //$.import_attribute,
        $.record_attribute,
        $.type_attribute,
        $.spec_attribute,
        $.other_attribute,
        $.no_paren_attribute
      ),

    module_attribute: ($) => seq("-", "module", "(", $._atom_or_macro, ")"),

    export_attribute: ($) =>
      seq("-", "export", "(", $.export_attribute_mfas, ")"),

    export_attribute_mfas: ($) =>
      seq("[", repeatComma1($.export_attribute_mfa), "]"),

    export_attribute_mfa: ($) =>
      seq($._atom_or_macro, "/", choice($.integer, $.macro)),

    record_attribute: ($) =>
      seq($.record_attribute_open, $._atom_or_macro, ",", $.record_fields, ")"),

    record_attribute_open: ($) => seq("-", "record", "("),

    record_fields: ($) => seq("{", repeatComma($.record_field), "}"),

    record_field: ($) =>
      seq(
        $.record_field_name_with_default_value,
        optional(seq($.type_bind_op, $._top_type))
      ),

    record_field_name_with_default_value: ($) =>
      seq($._atom_or_macro, optional(seq($.equal_op, $._expr))),

    type_attribute: ($) =>
      seq($.type_attribute_begin, $.type_bind_op, $._top_type),

    type_attribute_begin: ($) =>
      seq(
        "-",
        alias(token(choice("type", "opaque")), "type_or_opaque"),
        $.type_attribute_name,
        $.type_attribute_parameters
      ),

    type_attribute_name: ($) => $._atom_or_macro,

    type_attribute_parameters: ($) =>
      seq("(", repeatComma(choice($.variable, $.macro)), ")"),

    spec_attribute: ($) =>
      seq(
        "-",
        choice("spec", "callback"),
        choice($.type_spec, $.type_spec_with_paren)
      ),

    type_spec: ($) => seq($.spec_fun_name, $.type_sigs),

    type_spec_with_paren: ($) => seq("(", $.spec_fun_name, $.type_sigs, ")"),

    type_sigs: ($) => repeatSemicolon1($.type_sig),

    spec_fun_name: ($) =>
      seq($._atom_or_macro, optional(seq(":", $._atom_or_macro))),

    type_sig: ($) => seq($.fun_type, optional(seq("when", $.type_guards))),

    type_guards: ($) => repeatComma1($.type_guard),

    type_guard: ($) =>
      choice(seq($._atom_or_macro, "(", $.top_types, ")"), $.bind_type_guard),

    bind_type_guard: ($) =>
      seq(choice($.variable, $.macro), $.type_bind_op, $._top_type),

    fun_type: ($) => seq($.fun_type_open, $._top_type),

    fun_type_open: ($) =>
      seq("(", optional(choice($.top_types, "...")), ")", "->"),

    top_types: ($) => repeatComma1($._top_type),

    _top_type: ($) => choice($.binary_top_type, $.type),

    binary_top_type: ($) =>
      choice(
        seq(choice($.variable, $.macro), $.type_bind_op, $._top_type),
        prec(PREC.union, seq($.type, alias("|", "union_op"), $._top_type))
      ),

    type: ($) =>
      choice(
        $.binary_type,
        prec(PREC.prefix, seq($._prefix_op, $._top_type)),
        seq("(", $._top_type, ")"),
        $.variable,
        $._atom_or_macro,
        seq(
          $._atom_or_macro,
          optional(seq(":", $._atom_or_macro)),
          optional(seq("(", optional($.top_types), ")"))
        ),
        seq(
          "[",
          optional(
            seq($._top_type, optional(seq(",", alias("...", "list_type_tail"))))
          ),
          "]"
        ),
        seq("#", "{", optional($.map_field_types), "}"),
        seq("{", $.top_types, "}"),
        seq("#", $._atom_or_macro, "{", optional($.record_field_types), "}"),
        $.binary_expr_type,
        $.integer,
        $.char,
        seq("fun", "(", optional($.fun_type), ")")
      ),

    binary_type: ($) =>
      choice(
        prec.right(PREC.range, seq($.type, alias("..", "range_op"), $.type)),
        prec.right(PREC.add, seq($.type, $.add_op, $.type)),
        prec.right(PREC.mult, seq($.type, $.mult_op, $.type))
      ),

    map_field_types: ($) => repeatComma1($.map_field_type),

    map_field_type: ($) => seq($._top_type, $.map_type_op, $._top_type),

    record_field_types: ($) => repeatComma1($.record_field_type),

    record_field_type: ($) =>
      seq($._atom_or_macro, $.type_bind_op, $._top_type),

    binary_expr_type: ($) =>
      seq(
        "<<",
        optional(
          choice(
            $.binary_base_type,
            $.binary_unit_type,
            seq($.binary_base_type, ",", $.binary_unit_type)
          )
        ),
        ">>"
      ),

    binary_base_type: ($) => seq("_", ":", $.type),

    binary_unit_type: ($) => seq("_", ":", "_", "*", $.type),

    other_attribute: ($) =>
      seq($.other_attribute_open, repeatComma1($._expr), ")"),

    other_attribute_open: ($) => seq("-", $._atom_or_macro, "("),

    no_paren_attribute: ($) => seq("-", $._atom_or_macro),

    function_clauses: ($) => repeatSemicolon1($.function_clause),

    function_clause: ($) => seq($.function_clause_open, repeatComma1($._expr)),

    function_clause_open: ($) =>
      seq($._atom_or_macro, $.pat_parameters, optional($.clause_guard), "->"),

    pat_parameters: ($) => seq("(", repeatComma($._pat_expr), ")"),

    clause_guard: ($) => seq("when", $.guard),

    guard: ($) => repeatSemicolon1($.exprs),

    exprs: ($) => repeatComma1($._expr),

    _expr: ($) =>
      choice(
        $.binary_expr,
        $.unary_expr,
        $.map_expr,
        $.function_call,
        $.record_index_expr,
        $.record_expr,
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
        prec.right(PREC.comp, seq($._expr, $.comp_op, $._expr)),
        prec.right(PREC.list, seq($._expr, $.list_op, $._expr)),
        prec.right(PREC.add, seq($._expr, $.add_op, $._expr)),
        prec.right(PREC.mult, seq($._expr, $.mult_op, $._expr))
      ),

    unary_expr: ($) =>
      choice(
        prec(PREC.catch, seq("catch", $._expr)),
        prec(PREC.prefix, seq($._prefix_op, $._expr))
      ),

    map_expr: ($) => seq($.map_expr_open, repeatComma($.map_expr_field), "}"),

    map_expr_open: ($) =>
      seq(optional(choice($._primary_expr, $.map_expr)), "#", "{"),

    map_expr_field: ($) => seq($._expr, $.map_op, $._expr),

    function_call: ($) => seq($.function_call_open, repeatComma($._expr), ")"),

    function_call_open: ($) => seq(choice($.remote_expr, $._primary_expr), "("),

    record_index_expr: ($) =>
      seq(
        optional(choice($._primary_expr, $.record_expr)),
        "#",
        $._atom_or_macro,
        ".",
        $._atom_or_macro
      ),

    record_expr: ($) =>
      seq($.record_expr_open, repeatComma($.record_expr_field), "}"),

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
        $.list_comprehension,
        $.binary_comprehension,
        $.tuple,
        $.paren_expr,
        $.begin_end_expr,
        $.if_expr,
        $.case_expr,
        $.receive_expr,
        $.fun_ref_expr,
        $.fun_expr,
        $.fun_expr_with_head,
        $.try_expr
      ),

    list: ($) => choice(seq("[", repeatComma($._expr), $.list_close)),

    list_close: ($) => seq(optional(seq("|", $.list_tail)), "]"),

    list_tail: ($) => choice($._expr),

    binary: ($) => seq("<<", repeatComma($.binary_element), ">>"),

    binary_element: ($) =>
      seq(
        optional($._prefix_op),
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

    list_comprehension: ($) => seq("[", $.list_comprehension_content, "]"),

    list_comprehension_content: ($) =>
      seq($._expr, $.comprehension_op, $.list_comprehension_clauses),

    list_comprehension_clauses: ($) =>
      repeatComma1($.list_comprehension_clause),

    list_comprehension_clause: ($) =>
      seq($._pat_expr, "<-", $.comprehension_clause_expr),

    list_comprehension_clause_expr: ($) => $._expr,

    binary_comprehension: ($) =>
      seq("<<", $.binary_comprehension_content, ">>"),

    binary_comprehension_content: ($) =>
      seq($._expr, $.comprehension_op, $.binary_comprehension_clauses),

    binary_comprehension_clauses: ($) =>
      repeatComma1($.binary_comprehension_clause),

    binary_comprehension_clause: ($) =>
      seq($.pat_binary, "<=", $.comprehension_clause_expr),

    comprehension_clause_expr: ($) => $._expr,

    tuple: ($) => seq("{", repeatComma($._expr), "}"),

    paren_expr: ($) => seq("(", $._expr, ")"),

    begin_end_expr: ($) =>
      seq(
        alias("begin", "begin_open"),
        repeatComma1($._expr),
        alias("end", "end_close")
      ),

    if_expr: ($) => seq("if", $.if_expr_clauses, "end"),

    if_expr_clauses: ($) => repeatSemicolon1($.if_expr_clause),

    if_expr_clause: ($) => seq($.if_expr_clause_open, repeatComma1($._expr)),

    if_expr_clause_open: ($) => seq($.guard, "->"),

    case_expr: ($) => seq($.case_expr_begin, $.case_expr_clauses, "end"),

    case_expr_begin: ($) => seq("case", $.case_expr_begin_tail),

    case_expr_clauses: ($) => repeatSemicolon1($.match_clause),

    case_expr_begin_tail: ($) => seq($._expr, "of"),

    receive_expr: ($) =>
      seq(
        "receive",
        choice(
          seq($.receive_expr_clauses, "after", $.receive_expr_after),
          seq("after", $.receive_expr_after_clause),
          seq($.receive_expr_clauses)
        ),
        "end"
      ),

    receive_expr_clauses: ($) => repeatSemicolon1($.match_clause),

    receive_expr_after: ($) => $.receive_expr_after_clause,

    receive_expr_after_clause: ($) =>
      seq($.receive_expr_after_clause_open, repeatComma1($._expr)),

    receive_expr_after_clause_open: ($) => seq($._expr, "->"),

    try_expr: ($) =>
      seq(
        "try",
        $.try_expr_try,
        optional(seq("of", $.try_expr_of)),
        choice(
          seq("catch", $.try_expr_catch),
          seq("after", $.try_expr_after),
          seq("catch", $.try_expr_catch, "after", $.try_expr_after)
        ),
        "end"
      ),

    try_expr_try: ($) => repeatComma1($._expr),

    try_expr_of: ($) => repeatSemicolon1($.match_clause),

    try_expr_catch: ($) => repeatSemicolon1($.try_expr_catch_clause),

    try_expr_catch_clause: ($) =>
      seq($.try_expr_catch_clause_open, repeatComma1($._expr)),

    try_expr_catch_clause_open: ($) =>
      seq(
        choice(
          $._pat_expr,
          seq(
            choice($._atom_or_macro, $.variable),
            ":",
            $._pat_expr,
            optional(seq(":", choice($.variable, $.macro)))
          )
        ),
        "->"
      ),

    try_expr_after: ($) => repeatComma1($._expr),

    match_clause: ($) => seq($.match_clause_open, repeatComma1($._expr)),

    match_clause_open: ($) => seq($._pat_expr, optional($.clause_guard), "->"),

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
        alias("fun", "fun_open"),
        repeatSemicolon1($.fun_clause),
        alias("end", "end_close")
      ),

    fun_clause: ($) => seq($.fun_clause_open, repeatComma1($._expr)),

    fun_clause_open: ($) => seq($.pat_parameters, "->"),

    fun_expr_with_head: ($) =>
      seq(
        alias("fun", "fun_open"),
        repeatSemicolon1($.fun_clause_with_head),
        alias("end", "end_close")
      ),

    fun_clause_with_head: ($) =>
      seq($.fun_clause_with_head_open, repeatComma1($._expr)),

    fun_clause_with_head_open: ($) => seq($.variable, $.pat_parameters, "->"),

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
        prec.nonassoc(PREC.comp, seq($._pat_expr, $.comp_op, $._pat_expr)),
        prec.right(PREC.list, seq($._pat_expr, $.list_op, $._pat_expr)),
        prec.right(PREC.add, seq($._pat_expr, $.add_op, $._pat_expr)),
        prec.right(PREC.mult, seq($._pat_expr, $.mult_op, $._pat_expr))
      ),

    pat_unary_expr: ($) =>
      prec.right(PREC.prefix, seq($._prefix_op, $._pat_expr)),

    pat_map_expr: ($) =>
      seq($.pat_map_expr_open, repeatComma($.pat_map_expr_field), "}"),

    pat_map_expr_open: ($) =>
      seq(optional(choice($._pat_primary_expr, $.pat_map_expr)), "#", "{"),

    pat_map_expr_field: ($) => seq($._pat_expr, $.map_op, $._pat_expr),

    pat_record_index_expr: ($) =>
      seq("#", $._atom_or_macro, ".", $._atom_or_macro),

    pat_record_expr: ($) =>
      seq($.pat_record_expr_open, repeatComma($.pat_record_expr_field), "}"),

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
        seq("[", repeatComma1($._pat_expr), $.pat_list_close)
      ),

    pat_list_close: ($) => seq(optional(seq("|", $.pat_list_tail)), "]"),

    pat_list_tail: ($) => choice($._pat_expr),

    pat_binary: ($) => seq("<<", repeatComma($.pat_binary_element), ">>"),

    pat_binary_element: ($) =>
      seq(
        optional($._prefix_op),
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

    pat_tuple: ($) => seq("{", repeatComma($._pat_expr), "}"),

    pat_paren_expr: ($) => seq("(", $._pat_expr, ")"),

    _atomic: ($) =>
      choice($.char, $.integer, $.float, $._atom_or_macro, $.strings),

    _prefix_op: ($) => choice($.prefix_nospace_op, $.prefix_space_op),

    prefix_nospace_op: (_) => token(choice("+", "-")),

    prefix_space_op: (_) => token(choice("bnot", "not")),

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

    comprehension_op: (_) => "||",

    type_bind_op: (_) => "::",

    map_type_op: (_) => token(choice("=>", ":=")),

    variable: (_) => token(/[A-Z_][a-zA-Z0-9_]*/),

    _atom_or_macro: ($) => choice($.atom, $.macro),

    atom: (_) => token(choice(/'(\\.|[^'])*'/, /[a-z][a-zA-Z0-9_@]*/)),

    macro: (_) => token(/\?[a-zA-Z_][a-zA-Z0-9_@]*/),

    char: (_) => /\$./,

    integer: (_) =>
      token(
        choice(
          /-?(\d[\d_]*)?\d/,
          /-?(\d[\d_]*)?\d#([a-fA-F\d][a-fA-F\d_]*)?[a-fA-F\d]/
        )
      ),

    float: (_) => /-?(\d[\d_]*)?\d\.(\d[\d_]*)?\d(e-?(\d[\d_]*)?\d)?/,

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
