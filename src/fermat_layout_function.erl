-module(fermat_layout_function).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([text/2, juxtaposition/3, stacking/3, choice/3, layout_expr/2, span/2, value/2, gradient/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type layout_function() :: splay_tree:tree() | empty.
-type knot() :: non_neg_integer().
-type constants() :: {RightMargin :: non_neg_integer(), Alpha :: number(), Beta :: number()}.

-define(EPS, 0.0001).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc construct a layout function from a text layout expression
-spec text(fermat_layout_expr:layout_expr(), constants()) -> layout_function().
text({text, Text} = LayoutExpr, {RightMargin, _, Beta}) ->
    Span = length(Text),
    case Span < RightMargin of
        true -> splay_tree:from_list([{0, {LayoutExpr, Span, 0, 0}},
                                      {RightMargin - Span, {LayoutExpr, Span, 0, Beta}}]);
        false -> splay_tree:from_list([{0, {LayoutExpr, Span, (Span - RightMargin) * Beta, Beta}}])
    end.

-ifdef(EUNIT).
text_test_() ->
    [fun() ->
             LayoutExpr = {text, "foo"},
             RightMargin = 80,
             Alpha = 42,
             Beta = 57,
             ?assertEqual([{0, {LayoutExpr, 3, 0, 0}},
                           {RightMargin - 3, {LayoutExpr, 3, 0, Beta}}],
                          splay_tree:to_list(calc_layout_function(LayoutExpr, empty, {RightMargin, Alpha, Beta})))
     end,
     fun() ->
             LayoutExpr = {text, "foobar"},
             RightMargin = 4,
             Alpha = 42,
             Beta = 57,
             ?assertEqual([{0, {LayoutExpr, 6, 57 * 2, Beta}}],
                          splay_tree:to_list(calc_layout_function(LayoutExpr, empty, {RightMargin, Alpha, Beta})))
       end].
-endif.

%% @doc compose two layout functions with a stacking manner
-spec stacking(layout_function(), layout_function(), constants()) -> layout_function().
stacking(empty, RhsLayoutFunction, _) -> RhsLayoutFunction;
stacking(LhsLayoutFunction, empty, _) -> LhsLayoutFunction;
stacking(LhsLayoutFunction, RhsLayoutFunction, {_, Alpha, _}) ->
    Accumulator = fun(Knot, LayoutFunction) ->
                          splay_tree:store(Knot, {{stacking, layout_expr(Knot, LhsLayoutFunction), layout_expr(Knot, RhsLayoutFunction)},
                                                  span(Knot, RhsLayoutFunction),
                                                  value(Knot, LhsLayoutFunction) + value(Knot, RhsLayoutFunction) + Alpha,
                                                  gradient(Knot, LhsLayoutFunction) + gradient(Knot, RhsLayoutFunction)}, LayoutFunction)
                  end,
    Knots = sets:union(sets:from_list(splay_tree:keys(LhsLayoutFunction)), sets:from_list(splay_tree:keys(RhsLayoutFunction))),

    sets:fold(Accumulator, splay_tree:new(), Knots).

%% @doc compose two layout functions with a juxtaposition manner
-spec juxtaposition(layout_function(), layout_function(), constants()) -> layout_function().
juxtaposition(empty, RhsLayoutFunction, _) -> RhsLayoutFunction;
juxtaposition(LhsLayoutFunction, empty, _) -> LhsLayoutFunction;
juxtaposition(LhsLayoutFunction, RhsLayoutFunction, {RightMargin, _, Beta}) ->
    Accumulator =
        fun(Knot, LayoutFunction) ->
                RhsPosition = Knot + span(Knot, LhsLayoutFunction),
                splay_tree:store(Knot,
                                 {{juxtaposition, layout_expr(Knot, LhsLayoutFunction), layout_expr(RhsPosition, RhsLayoutFunction)},
                                  span(Knot, LhsLayoutFunction) + span(RhsPosition, RhsLayoutFunction),
                                  value(Knot, LhsLayoutFunction) + value(RhsPosition, RhsLayoutFunction) - Beta * max(RhsPosition - RightMargin, 0),
                                  gradient(Knot, LhsLayoutFunction) + gradient(RhsPosition, RhsLayoutFunction) - Beta * indicator(RhsPosition >= RightMargin)}, LayoutFunction)
        end,
    LhsKnots = splay_tree:keys(LhsLayoutFunction),
    RhsKnots = splay_tree:keys(RhsLayoutFunction),
    Knots = sets:union(sets:from_list(LhsKnots),
                       sets:from_list(lists:flatten([[Knot - Diff || Diff <- lists:seq(0, Knot), span(Knot - Diff, LhsLayoutFunction) =:= Diff] || Knot <- RhsKnots]))),

    sets:fold(Accumulator, splay_tree:new(), Knots).

%% @doc compose two layout functions with a choice manner
-spec choice(layout_function(), layout_function(), constants()) -> layout_function().
choice(empty, RhsLayoutFunction, _) -> RhsLayoutFunction;
choice(LhsLayoutFunction, empty, _) -> LhsLayoutFunction;
choice(LhsLayoutFunction, RhsLayoutFunction, _) ->
    KnotTree = sets:fold(fun(Knot, KnotTree) -> splay_tree:store(Knot, undefined, KnotTree) end,
                         splay_tree:new(),
                         sets:union(sets:from_list(splay_tree:keys(LhsLayoutFunction)),
                                    sets:from_list(splay_tree:keys(RhsLayoutFunction)))),

    AddingKnots = [AddingKnot || {Knot, NextKnot} <- slide(splay_tree:keys(KnotTree)),
                                 {ok, IntersectionPoint} <- [intersection_point(Knot, LhsLayoutFunction, RhsLayoutFunction)],
                                 AddingKnot <- [ceil(Knot + IntersectionPoint)], AddingKnot < NextKnot],

    AddedKnotTree = lists:foldl(fun(Knot, AccumulatedKnotTree) -> splay_tree:store(Knot, undefined, AccumulatedKnotTree) end, KnotTree, AddingKnots),

    splay_tree:map(fun(Knot, _) ->
                           case value(Knot, LhsLayoutFunction) < value(Knot, RhsLayoutFunction) orelse
                                (value(Knot, LhsLayoutFunction) == value(Knot, RhsLayoutFunction) andalso
                                 gradient(Knot, LhsLayoutFunction) =< gradient(Knot, RhsLayoutFunction)) of
                               true -> {layout_expr(Knot, LhsLayoutFunction), span(Knot, LhsLayoutFunction), value(Knot, LhsLayoutFunction), gradient(Knot, LhsLayoutFunction)};
                               false -> {layout_expr(Knot, RhsLayoutFunction), span(Knot, RhsLayoutFunction), value(Knot, RhsLayoutFunction), gradient(Knot, RhsLayoutFunction)}
                           end
                   end, AddedKnotTree).

-spec layout_expr(knot(), layout_function()) -> fermat_layout_expr:layout_expr().
layout_expr(X, LayoutFunction) ->
    {_, {LayoutExpr, _, _, _}} = greatest_lower_bound(X, LayoutFunction),
    LayoutExpr.

-spec span(knot(), layout_function()) -> integer().
span(X, LayoutFunction) ->
    {_, {_, Span, _, _}} = greatest_lower_bound(X, LayoutFunction),
    Span.

-spec value(knot(), layout_function()) -> number().
value(X, LayoutFunction) ->
    {Knot, {_, _, Intercept, Gradient}} = greatest_lower_bound(X, LayoutFunction),
    Intercept + Gradient * (X - Knot).

-spec gradient(knot(), layout_function()) -> number().
gradient(X, LayoutFunction) ->
    {_, {_, _, _, Gradient}} = greatest_lower_bound(X, LayoutFunction),
    Gradient.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec indicator(boolean()) -> 0 | 1.
indicator(false) -> 0;
indicator(true) -> 1.

-ifdef(EUNIT).
indicator_test() ->
    ?assertEqual(0, indicator(false)),
    ?assertEqual(1, indicator(true)).
-endif.

-spec greatest_lower_bound(knot(), layout_function()) -> {knot(), term()}. %TODO: more detail return type
greatest_lower_bound(X, LayoutFunction) ->
    case splay_tree:find(X, LayoutFunction) of
        {{ok, Value}, _} -> {X, Value};
        {error, _} ->
            {LeftTree, _} = splay_tree:split(X, LayoutFunction),
            case splay_tree:find_largest(LeftTree) of
                {{ok, Knot, Value}, _} -> {Knot, Value};
                {error, _} -> error(greatest_lower_bound_not_found)
            end
    end.

-ifdef(EUNIT).
greatest_lower_bound_test() ->
    LayoutFunction = splay_tree:from_list([{0, foo}, {1, bar}, {2, baz}]),

    ?assertError(greatest_lower_bound_not_found, greatest_lower_bound(-1, LayoutFunction)),
    ?assertEqual({0, foo}, greatest_lower_bound(0, LayoutFunction)),
    ?assertEqual({0, foo}, greatest_lower_bound(0.5, LayoutFunction)),
    ?assertEqual({1, bar}, greatest_lower_bound(1, LayoutFunction)),
    ?assertEqual({2, baz}, greatest_lower_bound(3, LayoutFunction)).
-endif.

-spec slide([knot()]) -> [{knot(), knot()}].
slide([]) -> [];
slide([_]) -> [];
slide([X | [Y | _] = XS]) -> [{X, Y} | slide(XS)].

-ifdef(EUNIT).
slide_test() ->
    ?assertEqual([{1, 2}, {2, 3}, {3, 4}], slide([1, 2, 3, 4])).
-endif.

-spec intersection_point(number(), layout_function(), layout_function()) -> {ok, number()} | {error, term()}.
intersection_point(X, LhsLayoutFunction, RhsLayoutFunction) ->
    LhsGradient = gradient(X, LhsLayoutFunction),
    RhsGradient = gradient(X, RhsLayoutFunction),

    case abs(LhsGradient - RhsGradient) < ?EPS of
        true -> {error, nearly_parallel};
        false ->
            LhsValue = value(X, LhsLayoutFunction),
            RhsValue = value(X, RhsLayoutFunction),

            IntersectionPoint = (RhsValue - LhsValue) / (LhsGradient - RhsGradient),

            case 0 < IntersectionPoint of
                true -> {ok, IntersectionPoint};
                false -> {error, negative_intersection_point}
            end
    end.

-ifdef(EUNIT).
intersection_point_test_() ->
    [fun() ->
             LhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 0, 1}}]),
             RhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 1, 1}}]),
             ?assertEqual({error, nearly_parallel}, intersection_point(0, LhsLayoutFunction, RhsLayoutFunction))
     end,
     fun() ->
             LhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 0, 0}}]),
             RhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 1, 1}}]),
             ?assertEqual({error, negative_intersection_point}, intersection_point(0, LhsLayoutFunction, RhsLayoutFunction))
     end,
     fun() ->
             LhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 2, 0}}]),
             RhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 0, 1}}]),
             ?assertEqual({ok, 2.0}, intersection_point(0, LhsLayoutFunction, RhsLayoutFunction))
     end,
     fun() ->
             LhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 2, 0}}]),
             RhsLayoutFunction = splay_tree:from_list([{0, {foo, bar, 0, 1}}]),
             ?assertEqual({ok, 1.0}, intersection_point(1, LhsLayoutFunction, RhsLayoutFunction))
     end].
-endif.
