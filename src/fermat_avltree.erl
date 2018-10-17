-module(fermat_avltree).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, insert/3, find/2, find_next/2, find_previous/2, keys/1, union/2, map/2, fold/3, from_list/1, to_list/1]).
-export_type([tree/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-record(node, {key :: term(),
               value :: term(),
               left = nil :: tree(),
               right = nil :: tree(),
               height = 1 :: non_neg_integer(),
               min_key :: term(),
               max_key :: term()}).

-type tree() :: #node{} | nil.
-type operation() :: insert_left | insert_right.
-type key() :: term().
-type value() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc
-spec new() -> tree().
new() -> nil.

-spec insert(key(), value(), tree()) -> tree().
insert(Key, Value, nil) ->
    #node{key = Key, value = Value, min_key = Key, max_key = Key};
insert(InsertingKey, InsertingValue, Tree = #node{key = Key,
                                                  left = Left,
                                                  right = Right}) ->
    if
        InsertingKey == Key ->
            Tree#node{value = InsertingValue};
        InsertingKey < Key ->
            balance(insert_left,
                    InsertingKey,
                    Tree#node{left = insert(InsertingKey,
                                            InsertingValue,
                                            Left)});
        InsertingKey > Key  ->
            balance(insert_right,
                    InsertingKey,
                    Tree#node{right = insert(InsertingKey,
                                             InsertingValue,
                                             Right)})
    end.

-ifdef(EUNIT).
insert_test() ->
    ?assertEqual(#node{key = 1,
                       value = a,
                       height = 1,
                       min_key = 1,
                       max_key = 1},
                 insert(1, a, new())),

    ?assertEqual(#node{key = 1,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 2,
                       right = #node{key = 2,
                                     value = a,
                                     height = 1,
                                     min_key = 2,
                                     max_key = 2}},
                 insert(2, a, insert(1, a, new()))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 2,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1}},
                 insert(1, a, insert(2, a, new()))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 insert(3, a, insert(2, a, insert(1, a, new())))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 insert(2, a, insert(3, a, insert(1, a, new())))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 insert(3, a, insert(1, a, insert(2, a, new())))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 insert(1, a, insert(3, a, insert(2, a, new())))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 insert(2, a, insert(1, a, insert(3, a, new())))),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 insert(1, a, insert(2, a, insert(3, a, new())))).
-endif.

-spec find(key(), tree()) -> {ok, value()} | {error, not_found}.
find(_, nil) -> {error, not_found};
find(FindingKey, #node{key = Key, value = Value, left = Left, right = Right}) ->
    if
        FindingKey == Key ->
            {ok, Value};
        FindingKey < Key ->
            find(FindingKey, Left);
        FindingKey > Key ->
            find(FindingKey, Right)
    end.

-ifdef(EUNIT).
find_test() ->
    Tree = from_list([{1, a}, {2, b}, {3, c}]),

    ?assertEqual({error, not_found}, find(0.5, Tree)),
    ?assertEqual({ok, a}, find(1.0, Tree)),
    ?assertEqual({error, not_found}, find(1.5, Tree)),
    ?assertEqual({ok, b}, find(2.0, Tree)),
    ?assertEqual({error, not_found}, find(2.5, Tree)),
    ?assertEqual({ok, c}, find(3.0, Tree)),
    ?assertEqual({error, not_found}, find(3.5, Tree)).
-endif.

-spec find_next(key(), tree()) -> {ok, {key(), value()}} | {error, not_found}.
find_next(_, nil) -> {error, not_found};
find_next(FindingKey, #node{key = Key, value = Value, left = Left, right = Right}) ->
    if
        FindingKey == Key ->
            {ok, {Key, Value}};
        FindingKey < Key ->
            case Left of
                nil ->
                    {ok, {Key, Value}};
                _ ->
                    case max_key(Left) < FindingKey of
                        true ->
                            {ok, {Key, Value}};
                        false ->
                            find_next(FindingKey, Left)
                    end
            end;
        FindingKey > Key ->
            find_next(FindingKey, Right)
    end.

-ifdef(EUNIT).
find_next_test() ->
    Tree = from_list([{1, a}, {2, a}, {3, a}]),

    ?assertEqual({ok, {1, a}}, find_next(0.5, Tree)),
    ?assertEqual({ok, {1, a}}, find_next(1.0, Tree)),
    ?assertEqual({ok, {2, a}}, find_next(1.5, Tree)),
    ?assertEqual({ok, {2, a}}, find_next(2.0, Tree)),
    ?assertEqual({ok, {3, a}}, find_next(2.5, Tree)),
    ?assertEqual({ok, {3, a}}, find_next(3.0, Tree)),
    ?assertEqual({error, not_found}, find_next(3.5, Tree)).
-endif.

-spec find_previous(key(), tree()) -> {ok, {key(), value()}} | {error, not_found}.
find_previous(_, nil) -> {error, not_found};
find_previous(FindingKey, #node{key = Key, value = Value, left = Left, right = Right}) ->
    if
        FindingKey == Key ->
            {ok, {Key, Value}};
        FindingKey < Key ->
            find_previous(FindingKey, Left);
        FindingKey > Key ->
            case Right of
                nil ->
                    {ok, {Key, Value}};
                _ ->
                    case FindingKey < min_key(Right) of
                        true ->
                            {ok, {Key, Value}};
                        false ->
                            find_previous(FindingKey, Right)
                    end
            end
    end.

-ifdef(EUNIT).
find_previous_test() ->
    Tree = from_list([{1, a}, {2, a}, {3, a}]),

    ?assertEqual({error, not_found}, find_previous(0.5, Tree)),
    ?assertEqual({ok, {1, a}}, find_previous(1.0, Tree)),
    ?assertEqual({ok, {1, a}}, find_previous(1.5, Tree)),
    ?assertEqual({ok, {2, a}}, find_previous(2.0, Tree)),
    ?assertEqual({ok, {2, a}}, find_previous(2.5, Tree)),
    ?assertEqual({ok, {3, a}}, find_previous(3.0, Tree)),
    ?assertEqual({ok, {3, a}}, find_previous(3.5, Tree)).
-endif.

-spec keys(tree()) -> [key()].
keys(nil) -> [];
keys(#node{key = Key, left = Left, right = Right}) ->
    keys(Left) ++ [Key] ++ keys(Right).

-ifdef(EUNIT).
keys_test() ->
    ?assertEqual([1, 2, 3, 4, 5, 6, 9], keys(from_list([{3, a}, {1, a}, {4, a}, {1, a}, {5, a}, {9, a}, {2, a}, {6, a}]))).
-endif.

-spec union(tree(), tree()) -> tree().
union(Tree, nil) -> Tree;
union(Tree, #node{key = Key,
                  value = Value,
                  left = Left,
                  right = Right}) ->
    union(insert(Key, Value, union(Tree, Left)), Right).

-ifdef(EUNIT).
union_test() ->
    ?assertEqual(#node{key = 3,
                       value = a,
                       height = 4,
                       min_key = 1,
                       max_key = 9,
                       left = #node{key = 1,
                                    value = b,
                                    height = 2,
                                    min_key = 1,
                                    max_key = 2,
                                    right = #node{key = 2,
                                                  value = b,
                                                  height = 1,
                                                  min_key = 2,
                                                  max_key = 2}},
                       right = #node{key = 7,
                                     value = b,
                                     height = 3,
                                     min_key = 4,
                                     max_key = 9,
                                     left = #node{key = 5,
                                                  value = a,
                                                  height = 2,
                                                  min_key = 4,
                                                  max_key = 6,
                                                  left = #node{key = 4,
                                                               value = a,
                                                               height = 1,
                                                               min_key = 4,
                                                               max_key = 4},
                                                  right = #node{key = 6,
                                                                value = a,
                                                                height = 1,
                                                                min_key = 6,
                                                                max_key = 6}},
                                     right = #node{key = 9,
                                                   value = a,
                                                   height = 2,
                                                   min_key = 8,
                                                   max_key = 9,
                                                   left = #node{key = 8,
                                                                value = b,
                                                                height = 1,
                                                                min_key = 8,
                                                                max_key = 8}}}},
                 union(from_list([{3, a}, {1, a}, {4, a}, {1, a}, {5, a}, {9, a}, {2, a}, {6, a}]),
                       from_list([{2, b}, {7, b}, {1, b}, {8, b}]))),
    ok.
-endif.

-spec map(fun((key(), value()) -> value()), tree()) -> tree().
map(_, nil) -> nil;
map(F, Tree = #node{key = Key,
                    value = Value,
                    left = Left,
                    right = Right}) ->
    Tree#node{left = map(F, Left),
              value = F(Key, Value),
              right = map(F, Right)}.

-ifdef(EUNIT).
map_test_() ->
    {setup,
     fun () -> ok end,
     fun (_) -> meck:unload() end,
     [fun () ->
              meck:new(fermat_avltree_dummy, [non_strict]),
              meck:expect(fermat_avltree_dummy, f, 2, b),

              ?assertEqual(#node{key = 2,
                                 value = b,
                                 height = 2,
                                 min_key = 1,
                                 max_key = 3,
                                 left = #node{key = 1,
                                              value = b,
                                              height = 1,
                                              min_key = 1,
                                              max_key = 1},
                                 right = #node{key = 3,
                                               value = b,
                                               height = 1,
                                               min_key = 3,
                                               max_key = 3}},
                           map(fun fermat_avltree_dummy:f/2, from_list([{1, a}, {2, a}, {3, a}]))),

              Pid = self(),
              ?assertEqual([{Pid, {fermat_avltree_dummy, f, [1, a]}, b},
                            {Pid, {fermat_avltree_dummy, f, [2, a]}, b},
                            {Pid, {fermat_avltree_dummy, f, [3, a]}, b}],
                           meck:history(fermat_avltree_dummy)),
              ok
      end]}.
-endif.

-spec fold(fun((key(), value(), A) -> A), A, tree()) -> A.
fold(_, Init, nil) -> Init;
fold(F, Init, #node{key = Key,
                    value = Value,
                    left = Left,
                    right = Right}) ->
    fold(F, F(Key, Value, fold(F, Init, Left)), Right).

-ifdef(EUNIT).
fold_test_() ->
    {setup,
     fun () -> ok end,
     fun (_) -> meck:unload() end,
     [fun () ->
              meck:new(fermat_avltree_dummy, [non_strict]),
              meck:expect(fermat_avltree_dummy, f, fun (Key, _Value, Accumulated) -> Accumulated + Key end),

              ?assertEqual(6, fold(fun fermat_avltree_dummy:f/3, 0, from_list([{1, a}, {2, a}, {3, a}]))),

              Pid = self(),
              ?assertEqual([{Pid, {fermat_avltree_dummy, f, [1, a, 0]}, 1},
                            {Pid, {fermat_avltree_dummy, f, [2, a, 1]}, 3},
                            {Pid, {fermat_avltree_dummy, f, [3, a, 3]}, 6}],
                           meck:history(fermat_avltree_dummy)),
              ok
      end]}.
-endif.

-spec from_list([{key(), value()}]) -> tree().
from_list(KeyValueList) ->
    lists:foldl(fun({Key, Value}, Accumulated) ->
                        insert(Key, Value, Accumulated)
                end, new(), KeyValueList).

-ifdef(EUNIT).
from_list_test() ->
    ?assertEqual(nil, from_list([])),

    ?assertEqual(#node{key = 1,
                       value = a,
                       height = 1,
                       min_key = 1,
                       max_key = 1},
                 from_list([{1, a}])),

    ?assertEqual(#node{key = 1,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 2,
                       right = #node{key = 2,
                                     value = a,
                                     height = 1,
                                     min_key = 2,
                                     max_key = 2}},
                 from_list([{1, a}, {2, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 2,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1}},
                 from_list([{2, a}, {1, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 from_list([{1, a}, {2, a}, {3, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 from_list([{1, a}, {3, a}, {2, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 from_list([{2, a}, {1, a}, {3, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 from_list([{2, a}, {3, a}, {1, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 from_list([{3, a}, {1, a}, {2, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 2,
                       min_key = 1,
                       max_key = 3,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 3,
                                     value = a,
                                     height = 1,
                                     min_key = 3,
                                     max_key = 3}},
                 from_list([{3, a}, {2, a}, {1, a}])),

    ?assertEqual(#node{key = 3,
                       value = a,
                       height = 4,
                       min_key = 1,
                       max_key = 9,
                       left = #node{key = 1,
                                    value = a,
                                    height = 2,
                                    min_key = 1,
                                    max_key = 2,
                                    right = #node{key = 2,
                                                  value = a,
                                                  height = 1,
                                                  min_key = 2,
                                                  max_key = 2}},
                       right = #node{key = 5,
                                     value = a,
                                     height = 3,
                                     min_key = 4,
                                     max_key = 9,
                                     left = #node{key = 4,
                                                  value = a,
                                                  height = 1,
                                                  min_key = 4,
                                                  max_key = 4},
                                     right = #node{key = 9,
                                                   value = a,
                                                   height = 2,
                                                   min_key = 6,
                                                   max_key = 9,
                                                   left = #node{key = 6,
                                                                value = a,
                                                                height = 1,
                                                                min_key = 6,
                                                                max_key = 6}}}},
                       from_list([{3, a}, {1, a}, {4, a}, {1, a}, {5, a}, {9, a}, {2, a}, {6, a}])),

    ?assertEqual(#node{key = 2,
                       value = a,
                       height = 3,
                       min_key = 1,
                       max_key = 8,
                       left = #node{key = 1,
                                    value = a,
                                    height = 1,
                                    min_key = 1,
                                    max_key = 1},
                       right = #node{key = 7,
                                     value = a,
                                     height = 2,
                                     min_key = 7,
                                     max_key = 8,
                                     right = #node{key = 8,
                                                   value = a,
                                                   height = 1,
                                                   min_key = 8,
                                                   max_key = 8}}},
                 from_list([{2, a}, {7, a}, {1, a}, {8, a}])).
-endif.

-spec to_list(tree()) -> [{key(), value()}].
to_list(nil) -> [];
to_list(#node{key = Key, value = Value, left = Left, right = Right}) ->
    to_list(Left) ++ [{Key, Value}] ++ to_list(Right).

-ifdef(EUNIT).
to_list_test() ->
    ?assertEqual([], to_list(from_list([]))),

    ?assertEqual([{1, a}], to_list(from_list([{1, a}]))),

    ?assertEqual([{1, a}, {2, a}], to_list(from_list([{1, a}, {2, a}]))),
    ?assertEqual([{1, a}, {2, a}], to_list(from_list([{2, a}, {1, a}]))),

    ?assertEqual([{1, a}, {2, a}, {3, a}], to_list(from_list([{1, a}, {2, a}, {3, a}]))),
    ?assertEqual([{1, a}, {2, a}, {3, a}], to_list(from_list([{1, a}, {3, a}, {2, a}]))),
    ?assertEqual([{1, a}, {2, a}, {3, a}], to_list(from_list([{2, a}, {1, a}, {3, a}]))),
    ?assertEqual([{1, a}, {2, a}, {3, a}], to_list(from_list([{2, a}, {3, a}, {1, a}]))),
    ?assertEqual([{1, a}, {2, a}, {3, a}], to_list(from_list([{3, a}, {1, a}, {2, a}]))),
    ?assertEqual([{1, a}, {2, a}, {3, a}], to_list(from_list([{3, a}, {2, a}, {1, a}]))).
-endif.

%%====================================================================
%% Internal functions
%%====================================================================
-spec balance(operation(), key(), tree()) -> tree().
balance(insert_left, InsertingKey, Tree = #node{left = Left,
                                                right = Right}) ->
    case height(Left) - height(Right) =:= 2 of
        true ->
            case InsertingKey < Left#node.key of
                true ->
                    left_left_rotation(Tree);
                false ->
                    left_right_rotation(Tree)
            end;
        false ->
            update(Tree)
    end;

balance(insert_right, InsertingKey, Tree = #node{left = Left,
                                                 right = Right}) ->
    case height(Right) - height(Left) =:= 2 of
        true ->
            case InsertingKey < Right#node.key of
                true ->
                    right_left_rotation(Tree);
                false ->
                    right_right_rotation(Tree)
            end;
        false ->
            update(Tree)
    end.

-spec left_left_rotation(tree()) -> tree().
left_left_rotation(Tree = #node{left = L = #node{right = LR}}) ->
    update(L#node{right = update(Tree#node{left = LR})}).

-spec left_right_rotation(tree()) -> tree().
left_right_rotation(Tree = #node{left = L = #node{right = LR = #node{left = LRL, right = LRR}}}) ->
    update(LR#node{left = update(L#node{right = LRL}),
                   right = update(Tree#node{left = LRR})}).

-spec right_left_rotation(tree()) -> tree().
right_left_rotation(Tree = #node{right = R = #node{left = RL = #node{left = RLL, right = RLR}}}) ->
    update(RL#node{left = update(Tree#node{right = RLL}),
                   right = update(R#node{left = RLR})}).

-spec right_right_rotation(tree()) -> tree().
right_right_rotation(Tree = #node{right = R = #node{left = RL}}) ->
    update(R#node{left = update(Tree#node{right = RL})}).

-spec update(tree()) -> tree().
update(Tree = #node{key = Key, left = Left, right = Right}) ->
    Tree#node{height = max(height(Left), height(Right)) + 1,
              min_key = case Left  of nil -> Key; _ -> min_key(Left) end,
              max_key = case Right of nil -> Key; _ -> max_key(Right) end}.

-spec height(tree()) -> non_neg_integer().
height(nil) -> 0;
height(#node{height = Height}) -> Height.

-spec min_key(tree()) -> key().
min_key(#node{min_key = MinKey}) -> MinKey.

-spec max_key(tree()) -> {key(), value()}.
max_key(#node{max_key = MaxKey}) -> MaxKey.
