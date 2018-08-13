-module(fermat_formatter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type environment() :: #{fermat_layout_expr:var_id() => fermat_layout_function:layout_function()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format(fermat_layout_expr:layout_expr(), non_neg_integer(), number(), number()) -> fermat_layout_expr:layout_expr().
format(LayoutExpr, RightMargin, Alpha, Beta) ->
    LayoutFunction = construct_layout_function(LayoutExpr, empty, #{}, {RightMargin, Alpha, Beta}),
    fermat_layout_function:layout_expr(0, LayoutFunction).

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
    construct_layout_function(compile_wrap(LayoutExprs, 1, length(LayoutExprs), fermat_layout_expr:var(1)), TrailingLayoutFunction, Environment, Constants);

construct_layout_function({half_indent, LayoutExpr}, TrailingLayoutFunction, Environment, Constants) ->
    construct_layout_function({juxtaposition, {text, "  "}, LayoutExpr}, TrailingLayoutFunction, Environment, Constants);

construct_layout_function({indent, LayoutExpr}, TrailingLayoutFunction, Environment, Constants) ->
    construct_layout_function({juxtaposition, {text, "    "}, LayoutExpr}, TrailingLayoutFunction, Environment, Constants);

construct_layout_function({bind, VarId, LayoutExpr, BodyLayoutExpr}, TrailingLayoutFunction, Environment, Constants) ->
    LayoutFunction = construct_layout_function(LayoutExpr, TrailingLayoutFunction, Environment, Constants),
    construct_layout_function(BodyLayoutExpr, TrailingLayoutFunction, Environment#{VarId => LayoutFunction}, Constants);

construct_layout_function({var, VarId}, _TrailingLayoutFunction, Environment, _Constants) ->
    case Environment of
        #{VarId := LayoutFunction} -> LayoutFunction
    end.

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

