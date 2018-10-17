-module(fermat_layout_function).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([text/2, juxtaposition/3, stacking/3, choice/3, at/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type layout_function() :: fermat_avltree:tree() | empty.
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
        true -> fermat_avltree:from_list([{0, {LayoutExpr, Span, 0, 0}},
                                          {RightMargin - Span, {LayoutExpr, Span, 0, Beta}}]);
        false -> fermat_avltree:from_list([{0, {LayoutExpr, Span, (Span - RightMargin) * Beta, Beta}}])
    end.

%% @doc compose two layout functions with a stacking manner
-spec stacking(layout_function(), layout_function(), constants()) -> layout_function().
stacking(empty, RhsLayoutFunction, _) -> RhsLayoutFunction;
stacking(LhsLayoutFunction, empty, _) -> LhsLayoutFunction;
stacking(LhsLayoutFunction, RhsLayoutFunction, {_, Alpha, _}) ->
    fermat_avltree:map(fun(Knot, _) ->
                               {LhsLayoutExpr, _, LhsValue, LhsGradient} = at(Knot, LhsLayoutFunction),
                               {RhsLayoutExpr, RhsSpan, RhsValue, RhsGradient} = at(Knot, RhsLayoutFunction),
                               {fermat_layout_expr:stacking(LhsLayoutExpr, RhsLayoutExpr),
                                RhsSpan,
                                LhsValue + RhsValue + Alpha,
                                LhsGradient + RhsGradient}
                       end, fermat_avltree:union(LhsLayoutFunction, RhsLayoutFunction)).

%% @doc compose two layout functions with a juxtaposition manner
-spec juxtaposition(layout_function(), layout_function(), constants()) -> layout_function().
juxtaposition(empty, RhsLayoutFunction, _) -> RhsLayoutFunction;
juxtaposition(LhsLayoutFunction, empty, _) -> LhsLayoutFunction;
juxtaposition(LhsLayoutFunction, RhsLayoutFunction, {RightMargin, _, Beta}) ->
    KnotsAddedLayoutFunction = fermat_avltree:fold(
                                 fun (RhsKnot, _, AccumulatedByRhsKnots) ->
                                         fermat_avltree:fold(
                                           fun (LhsKnot, {_, Span, _, _}, AccumulatedByLhsKnots) ->
                                                   case fermat_avltree:find_next(LhsKnot + 1, LhsLayoutFunction) of
                                                       {ok, {NextLhsKnot, _}} ->
                                                           KnotCandidate = RhsKnot - Span,
                                                           case LhsKnot =< KnotCandidate andalso KnotCandidate < NextLhsKnot of
                                                               true ->
                                                                   fermat_avltree:insert(KnotCandidate, undefined, AccumulatedByLhsKnots);
                                                               false ->
                                                                   AccumulatedByLhsKnots
                                                           end;
                                                       {error, not_found} ->
                                                           AccumulatedByLhsKnots
                                                   end
                                           end,
                                           AccumulatedByRhsKnots,
                                           LhsLayoutFunction)
                                 end,
                                 LhsLayoutFunction,
                                 RhsLayoutFunction),

    fermat_avltree:map(fun(Knot, _) ->
                               {LhsLayoutExpr, LhsSpan, LhsValue, LhsGradient} = at(Knot, LhsLayoutFunction),
                               RhsPosition = Knot + LhsSpan,
                               {RhsLayoutExpr, RhsSpan, RhsValue, RhsGradient} = at(RhsPosition, RhsLayoutFunction),
                               {fermat_layout_expr:juxtaposition(LhsLayoutExpr, RhsLayoutExpr),
                                LhsSpan + RhsSpan,
                                LhsValue + RhsValue - Beta * max(RhsPosition - RightMargin, 0),
                                LhsGradient + RhsGradient - Beta * indicator(RhsPosition >= RightMargin)}
                       end, KnotsAddedLayoutFunction).

%% @doc compose two layout functions with a choice manner
-spec choice(layout_function(), layout_function(), constants()) -> layout_function().
choice(empty, RhsLayoutFunction, _) -> RhsLayoutFunction;
choice(LhsLayoutFunction, empty, _) -> LhsLayoutFunction;
choice(LhsLayoutFunction, RhsLayoutFunction, _) ->
    UnitedLayoutFunction = fermat_avltree:union(LhsLayoutFunction, RhsLayoutFunction),

    KnotsAddedLayoutFunction = fermat_avltree:union(
                                 UnitedLayoutFunction,
                                 fermat_avltree:from_list(
                                   [{AddingKnot, undefined} || {Knot, NextKnot} <- slide(fermat_avltree:keys(UnitedLayoutFunction)),
                                                               {ok, IntersectionPoint} <- [intersection_point(Knot, LhsLayoutFunction, RhsLayoutFunction)],
                                                               AddingKnot <- [ceil(Knot + IntersectionPoint)], AddingKnot < NextKnot])),

    fermat_avltree:map(fun(Knot, _) ->
                               {LhsLayoutExpr, LhsSpan, LhsValue, LhsGradient} = at(Knot, LhsLayoutFunction),
                               {RhsLayoutExpr, RhsSpan, RhsValue, RhsGradient} = at(Knot, RhsLayoutFunction),
                               case LhsValue < RhsValue orelse
                                    (LhsValue == RhsValue andalso
                                     LhsGradient =< RhsGradient) of
                                   true -> {LhsLayoutExpr, LhsSpan, LhsValue, LhsGradient};
                                   false -> {RhsLayoutExpr, RhsSpan, RhsValue, RhsGradient}
                               end
                       end, KnotsAddedLayoutFunction).

-spec at(non_neg_integer(), layout_function()) -> {fermat_layout_expr:layout_expr(), non_neg_integer(), number(), number()}.
at(X, LayoutFunction) ->
    case fermat_avltree:find_previous(X, LayoutFunction) of
        {ok, {Knot, {LayoutExpr, Span, Intercept, Gradient}}} ->
            {LayoutExpr, Span, Intercept + Gradient * (X - Knot), Gradient};
        {error, not_found} -> error(not_found)
    end.

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
    {_, _, LhsValue, LhsGradient} = at(X, LhsLayoutFunction),
    {_, _, RhsValue, RhsGradient} = at(X, RhsLayoutFunction),

    case abs(LhsGradient - RhsGradient) < ?EPS of
        true -> {error, nearly_parallel};
        false ->
            IntersectionPoint = (RhsValue - LhsValue) / (LhsGradient - RhsGradient),

            case 0 < IntersectionPoint of
                true -> {ok, IntersectionPoint};
                false -> {error, negative_intersection_point}
            end
    end.

-ifdef(EUNIT).
intersection_point_test() ->
    ?assertEqual({error, nearly_parallel},
                 intersection_point(0,
                                    fermat_avltree:from_list([{0, {foo, bar, 0, 1}}]),
                                    fermat_avltree:from_list([{0, {foo, bar, 1, 1}}]))),

    ?assertEqual({error, negative_intersection_point},
                 intersection_point(0,
                                    fermat_avltree:from_list([{0, {foo, bar, 0, 0}}]),
                                    fermat_avltree:from_list([{0, {foo, bar, 1, 1}}]))),

    ?assertEqual({ok, 2.0},
                 intersection_point(0,
                                    fermat_avltree:from_list([{0, {foo, bar, 2, 0}}]),
                                    fermat_avltree:from_list([{0, {foo, bar, 0, 1}}]))),

    ?assertEqual({ok, 1.0},
                 intersection_point(1,
                                    fermat_avltree:from_list([{0, {foo, bar, 2, 0}}]),
                                    fermat_avltree:from_list([{0, {foo, bar, 0, 1}}]))).
-endif.
