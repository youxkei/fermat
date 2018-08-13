-module(fermat_layout_expr).
-compile([nowarn_unused_vars]).
-compile([nowarn_unused_function]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([parse/1, text/1, juxtaposition/2, stacking/2, choice/2, wrap/1, indent/1, half_indent/1, bind/3, var/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([layout_expr/0, var_id/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type var_id() :: non_neg_integer().

-type layout_expr() :: empty 
                     | {text, string()}
                     | {juxtaposition, layout_expr(), layout_expr()}
                     | {stacking, layout_expr(), layout_expr()}
                     | {choice, layout_expr(), layout_expr()}
                     | {wrap, [layout_expr()]}
                     | {indent, layout_expr()}
                     | {half_indent, layout_expr()}
                     | {bind, var_id(), layout_expr(), layout_expr()}
                     | {var, var_id()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec parse([fermat_token:token()]) -> {ok, layout_expr()} | {error, term()}.
parse(Tokens) ->
    {LayoutExpr, []} = parse_stack(Tokens, false),
    {ok, LayoutExpr}.

-spec text(string()) -> layout_expr().
text(Text) -> {text, Text}.

-spec juxtaposition(layout_expr(), layout_expr()) -> layout_expr().
juxtaposition(empty, Rhs) -> Rhs;
juxtaposition(Lhs, empty) -> Lhs;
juxtaposition(Lhs, Rhs) -> {juxtaposition, Lhs, Rhs}.

-spec stacking(layout_expr(), layout_expr()) -> layout_expr().
stacking(empty, Rhs) -> Rhs;
stacking(Lhs, empty) -> Lhs;
stacking(Lhs, Rhs) -> {stacking, Lhs, Rhs}.

-spec choice(layout_expr(), layout_expr()) -> layout_expr().
choice(empty, Rhs) -> Rhs;
choice(Lhs, empty) -> Lhs;
choice(Lhs, Rhs) -> {choice, Lhs, Rhs}.

-spec wrap([layout_expr()]) -> layout_expr().
wrap(LayoutExprs) ->
    case [LayoutExpr || LayoutExpr <- LayoutExprs, LayoutExpr =/= empty] of
        [] -> empty;
        NonemptyLayoutExprs -> {wrap, NonemptyLayoutExprs}
    end.

-spec indent(layout_expr()) -> layout_expr().
indent(empty) -> empty;
indent(LayoutExpr) -> {indent, LayoutExpr}.

-spec half_indent(layout_expr()) -> layout_expr().
half_indent(empty) -> empty;
half_indent(LayoutExpr) -> {half_indent, LayoutExpr}.

-spec bind(var_id(), layout_expr(), layout_expr()) -> layout_expr().
bind(VarId, LayoutExpr, empty) -> empty;
bind(VarId, LayoutExpr, BodyLayoutExpr) ->
    {bind, VarId, LayoutExpr, BodyLayoutExpr}.

-spec var(var_id()) -> layout_expr().
var(VarId) -> {var, VarId}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec parse_stack([fermat_token:token()], boolean()) -> {layout_expr(), [fermat_token:token()]}.
parse_stack([], _) -> {empty, []};
parse_stack(Tokens, EndWithDot) ->
    {Line, Terminator, RestTokens} = parse_juxtaposition(Tokens, empty),

    case fermat_token:category(Terminator) of
        dot when EndWithDot -> {Line, RestTokens};

        Category when Category =:= ';' orelse Category =:= 'end' ->
            {Line, RestTokens};

        '->' ->
            {Stack, RestTokensOfStackParsing} = parse_stack(RestTokens, true),
            {RestStack, RestTokensOfRestStackParsing} = parse_stack(RestTokensOfStackParsing, EndWithDot),

            {stacking(choice(stacking(Line, indent(Stack)),
                          juxtaposition(Line, Stack)),
                   RestStack),
             RestTokensOfRestStackParsing};

        _ ->
            {RestStack, RestTokensOfRestStackParsing} = parse_stack(RestTokens, EndWithDot),

            {stacking(Line, RestStack), RestTokensOfRestStackParsing}
    end.

-spec parse_juxtaposition([fermat_token:token()], layout_expr()) -> {layout_expr(), fermat_token:token(), [fermat_token:token()]}.
parse_juxtaposition([], _) -> error(unexpected_eof);
parse_juxtaposition([HeadToken|RestTokens] = Tokens, AccumulatedLayoutExpr) ->
    case fermat_token:category(HeadToken) of
        Category when Category =:= dot orelse
                      Category =:= 'of' orelse
                      Category =:= ';' orelse
                      Category =:= ',' orelse
                      Category =:= '->' ->
            {juxtaposition(AccumulatedLayoutExpr, text(fermat_token:text(HeadToken))), HeadToken, RestTokens};

        Category when Category =:= 'end' orelse
                      Category =:= ')' orelse
                      Category =:= '}' orelse
                      Category =:= ']' ->
            {AccumulatedLayoutExpr, HeadToken, Tokens};

        OpenCategory when OpenCategory =:= '(' orelse
                          OpenCategory =:= '{' orelse
                          OpenCategory =:= '[' ->
            {Wrap, [CloseToken|RestTokensOfWrapParsing]} = parse_wrap(RestTokens, []),
            OpenText = text(fermat_token:text(HeadToken)),
            CloseText = text(fermat_token:text(CloseToken)),

            case Wrap of
                empty ->
                    parse_juxtaposition(RestTokensOfWrapParsing,
                               juxtaposition(juxtaposition(AccumulatedLayoutExpr, OpenText), CloseText));
                _ ->
                    parse_juxtaposition(RestTokensOfWrapParsing,
                               choice(stacking(juxtaposition(AccumulatedLayoutExpr, OpenText), half_indent(juxtaposition(Wrap, CloseText))),
                                      juxtaposition(juxtaposition(juxtaposition(AccumulatedLayoutExpr, OpenText), Wrap), CloseText)))
            end;

        'fun' ->
            {Stack, [EndToken|RestTokensOfFunStackParsing]} = parse_stack(RestTokens, true),
            FunText = text(fermat_token:text(HeadToken)),
            EndText = text(fermat_token:text(EndToken)),

            parse_juxtaposition(RestTokensOfFunStackParsing,
                       juxtaposition(AccumulatedLayoutExpr, choice(stacking(juxtaposition(FunText, Stack), EndText),
                                                          juxtaposition(juxtaposition(FunText, Stack), EndText))));

        'case' ->
            {CaseLine, _OfToken, RestTokensOfCaseLineParsing} = parse_juxtaposition(RestTokens, empty),
            {CaseStack, [EndToken|RestTokensOfCaseStackParsing]} = parse_stack(RestTokensOfCaseLineParsing, true),
            CaseText = text(fermat_token:text(HeadToken)),
            EndText = text(fermat_token:text(EndToken)),

            parse_juxtaposition(RestTokensOfCaseStackParsing,
                       juxtaposition(AccumulatedLayoutExpr,
                            stacking(juxtaposition(CaseText, CaseLine),
                                  stacking(indent(CaseStack), EndText))));

        _ -> parse_juxtaposition(RestTokens, juxtaposition(AccumulatedLayoutExpr, text(fermat_token:text(HeadToken))))
    end.

-spec parse_wrap([fermat_token:token()], [layout_expr()]) -> {layout_expr(), [fermat_token:token()]}.
parse_wrap(Tokens, AccumulatedLayoutExprs) ->
    {Line, Terminator, RestTokens} = parse_juxtaposition(Tokens, empty),
    case fermat_token:category(Terminator) of
        Category when Category =:= ')' orelse
                      Category =:= '}' orelse
                      Category =:= ']' ->
            {wrap(lists:reverse([Line|AccumulatedLayoutExprs])), RestTokens};
        ',' -> parse_wrap(RestTokens, [Line|AccumulatedLayoutExprs]);
        _ -> error(close_expected)
    end.

