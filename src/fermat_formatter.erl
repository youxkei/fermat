-module(fermat_formatter).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type environment() :: #{fermat_layout_expr:var_id() => fermat_layout_function:layout_function()}.

-define(USE_SMART_WRAP, false).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format(fermat_layout_expr:layout_expr(), non_neg_integer(), number(), number()) -> fermat_layout_expr:layout_expr().
format(LayoutExpr, RightMargin, Alpha, Beta) ->
    LayoutFunction = construct_layout_function(LayoutExpr, empty, #{}, {RightMargin, Alpha, Beta}),
    {FormattedLayoutExpr, _, _, _} = fermat_layout_function:at(0, LayoutFunction),
    FormattedLayoutExpr.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec construct_layout_function(fermat_layout_expr:layout_expr(), fermat_layout_function:layout_function(), environment(), fermat_layout_function:constants()) -> fermat_layout_function:layout_function().
construct_layout_function({text, _} = LayoutExpr, TrailingLayoutFunction, _Environment, Constants) ->
    fermat_layout_function:juxtaposition(fermat_layout_function:text(LayoutExpr, Constants), TrailingLayoutFunction, Constants);

construct_layout_function({stacking, Lhs, Rhs}, TrailingLayoutFunction, Environment, Constants) ->
    fermat_layout_function:stacking(construct_layout_function(Lhs, empty, Environment, Constants),
                                    construct_layout_function(Rhs, TrailingLayoutFunction, Environment, Constants), Constants);

construct_layout_function({juxtaposition, Lhs, Rhs}, TrailingLayoutFunction, Environment, Constants) ->
    construct_layout_function(Lhs, construct_layout_function(Rhs, TrailingLayoutFunction, Environment, Constants), Environment, Constants);

construct_layout_function({choice, Lhs, Rhs}, TrailingLayoutFunction, Environment, Constants) ->
    fermat_layout_function:choice(construct_layout_function(Lhs, TrailingLayoutFunction, Environment, Constants),
                                  construct_layout_function(Rhs, TrailingLayoutFunction, Environment, Constants), Constants);

construct_layout_function({wrap, LayoutExprs}, TrailingLayoutFunction, Environment, Constants) ->
    case ?USE_SMART_WRAP of
        true ->
            construct_layout_function(compile_wrap(LayoutExprs, 1, length(LayoutExprs), fermat_layout_expr:var(1)), TrailingLayoutFunction, Environment, Constants);
        false ->
            case LayoutExprs of
                [Head|Tail] ->
                    Stacked = lists:foldl(fun (LayoutExpr, AccumulatedLayoutExpr) -> fermat_layout_expr:stacking(AccumulatedLayoutExpr, LayoutExpr) end, Head, Tail),
                    Juxtaposed = lists:foldl(fun (LayoutExpr, AccumulatedLayoutExpr) -> fermat_layout_expr:juxtaposition(AccumulatedLayoutExpr, LayoutExpr) end, Head, Tail),

                    construct_layout_function(fermat_layout_expr:choice(Stacked, Juxtaposed), TrailingLayoutFunction, Environment, Constants);
                [] ->
                    construct_layout_function(fermat_layout_expr:text(""), TrailingLayoutFunction, Environment, Constants)
            end
    end;

construct_layout_function({half_indent, LayoutExpr}, TrailingLayoutFunction, Environment, Constants) ->
    construct_layout_function(fermat_layout_expr:juxtaposition(fermat_layout_expr:text("  "), LayoutExpr), TrailingLayoutFunction, Environment, Constants);

construct_layout_function({indent, LayoutExpr}, TrailingLayoutFunction, Environment, Constants) ->
    construct_layout_function(fermat_layout_expr:juxtaposition(fermat_layout_expr:text("    "), LayoutExpr), TrailingLayoutFunction, Environment, Constants);

construct_layout_function({bind, VarId, LayoutExpr, BodyLayoutExpr}, TrailingLayoutFunction, Environment, Constants) ->
    LayoutFunction = construct_layout_function(LayoutExpr, TrailingLayoutFunction, Environment, Constants),
    construct_layout_function(BodyLayoutExpr, TrailingLayoutFunction, Environment#{VarId => LayoutFunction}, Constants);

construct_layout_function({var, VarId}, _TrailingLayoutFunction, Environment, _Constants) ->
    case Environment of
        #{VarId := LayoutFunction} -> LayoutFunction
    end.

-ifdef(EUNIT).
text_test_() ->
    [fun() ->
             LayoutExpr = {text, "foo"},
             RightMargin = 80,
             Alpha = 97,
             Beta = 89,
             ?assertEqual([{0, {LayoutExpr, 3, 0, 0}},
                           {RightMargin - 3, {LayoutExpr, 3, 0, Beta}}],
                          fermat_avltree:to_list(construct_layout_function(LayoutExpr, empty, #{}, {RightMargin, Alpha, Beta})))
     end,
     fun() ->
             LayoutExpr = {text, "foobar"},
             RightMargin = 4,
             Alpha = 97,
             Beta = 89,
             ?assertEqual([{0, {LayoutExpr, 6, Beta * 2, Beta}}],
                          fermat_avltree:to_list(construct_layout_function(LayoutExpr, empty, #{}, {RightMargin, Alpha, Beta})))
     end].
-endif.

compile_wrap([], _CurrentIndex, _N, AccumulatedWrapExpr) -> AccumulatedWrapExpr;

compile_wrap([_|_] = LayoutExprs, CurrentIndex, N, AccumulatedWrapExpr) ->
    Prefixes = lists:foldl(fun(LayoutExpr, AccumulatedPrefixes) ->
                                           [[LayoutExpr] | [[LayoutExpr|Prefix] || Prefix <- AccumulatedPrefixes]]
                                   end, [], lists:reverse(LayoutExprs)),

    {_, LayoutExpr} = lists:foldl(fun(Prefix, {Index, AccumulatedLayoutExpr}) ->
                                          CombinedWithLine = lists:foldl(
                                                               fun(LayoutExpr, AccumulatedLine) ->
                                                                       fermat_layout_expr:juxtaposition(AccumulatedLine, LayoutExpr)
                                                               end, empty, Prefix),
                                          case Index =:= N of
                                              true ->
                                                  {Index + 1, fermat_layout_expr:choice(AccumulatedLayoutExpr, CombinedWithLine)};
                                              false ->
                                                  {Index + 1, fermat_layout_expr:choice(AccumulatedLayoutExpr, fermat_layout_expr:stacking(CombinedWithLine, fermat_layout_expr:var(Index + 1)))}
                                          end
                                  end, {CurrentIndex, empty}, Prefixes),

    compile_wrap(tl(LayoutExprs), CurrentIndex + 1, N, fermat_layout_expr:bind(CurrentIndex, LayoutExpr, AccumulatedWrapExpr)).

-ifdef(EUNIT).
compile_wrap_test() ->
    A = fermat_layout_expr:text("a"),
    B = fermat_layout_expr:text("b"),
    C = fermat_layout_expr:text("c"),
    D = fermat_layout_expr:text("d"),

    ?assertEqual(fermat_layout_expr:bind(
                   1, A,
                   fermat_layout_expr:var(1)),
                 compile_wrap([A], 1, 1, fermat_layout_expr:var(1))),

    ?assertEqual(fermat_layout_expr:bind(
                   2, B,
                   fermat_layout_expr:bind(
                     1, fermat_layout_expr:choice(
                          fermat_layout_expr:stacking(A, fermat_layout_expr:var(2)),
                          fermat_layout_expr:juxtaposition(A, B)),
                     fermat_layout_expr:var(1))),
                 compile_wrap([A, B], 1, 2, fermat_layout_expr:var(1))),

    ?assertEqual(fermat_layout_expr:bind(
                   3, C,
                   fermat_layout_expr:bind(
                     2, fermat_layout_expr:choice(
                          fermat_layout_expr:stacking(B, fermat_layout_expr:var(3)),
                          fermat_layout_expr:juxtaposition(B, C)),
                     fermat_layout_expr:bind(
                       1, fermat_layout_expr:choice(
                            fermat_layout_expr:choice(
                              fermat_layout_expr:stacking(A, fermat_layout_expr:var(2)),
                              fermat_layout_expr:stacking(
                                fermat_layout_expr:juxtaposition(A, B),
                                fermat_layout_expr:var(3))),
                            fermat_layout_expr:juxtaposition(
                              fermat_layout_expr:juxtaposition(A, B), C)),
                       fermat_layout_expr:var(1)))),
                 compile_wrap([A, B, C], 1, 3, fermat_layout_expr:var(1))),

    ?assertEqual(fermat_layout_expr:bind(
                   4, D,
                   fermat_layout_expr:bind(
                     3, fermat_layout_expr:choice(
                          fermat_layout_expr:stacking(C, fermat_layout_expr:var(4)),
                          fermat_layout_expr:juxtaposition(C, D)),
                     fermat_layout_expr:bind(
                       2, fermat_layout_expr:choice(
                            fermat_layout_expr:choice(
                              fermat_layout_expr:stacking(B, fermat_layout_expr:var(3)),
                              fermat_layout_expr:stacking(
                                fermat_layout_expr:juxtaposition(B, C),
                                fermat_layout_expr:var(4))),
                            fermat_layout_expr:juxtaposition(
                              fermat_layout_expr:juxtaposition(B, C), D)),
                       fermat_layout_expr:bind(
                         1, fermat_layout_expr:choice(
                              fermat_layout_expr:choice(
                                fermat_layout_expr:choice(
                                  fermat_layout_expr:stacking(A, fermat_layout_expr:var(2)),
                                  fermat_layout_expr:stacking(
                                    fermat_layout_expr:juxtaposition(A, B),
                                    fermat_layout_expr:var(3))),
                                fermat_layout_expr:stacking(
                                  fermat_layout_expr:juxtaposition(
                                    fermat_layout_expr:juxtaposition(A, B), C),
                                  fermat_layout_expr:var(4))),
                              fermat_layout_expr:juxtaposition(
                                fermat_layout_expr:juxtaposition(
                                  fermat_layout_expr:juxtaposition(A, B), C), D)),
                         fermat_layout_expr:var(1))))),
                 compile_wrap([A, B, C, D], 1, 4, fermat_layout_expr:var(1))),

    ok.
-endif.
