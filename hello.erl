%%                                               |
-module(hello).
-export([
         main/0,

        ]).
-include_lib("somelib",).



%% ハローワールド
-spec main(Var) -> Var when Var :: Var, Var :: Var,;
          (1 + 2) -> 3 * 4 .. 5;
          (?VAR) -> ?VAR when ?VAR :: ?VAR, ?VAR :: ?VAR,;.
-spec(main(Var) -> Var when Var :: Var, Var :: Var,;
          (?VAR) -> ?VAR when ?VAR :: ?VAR, ?VAR :: ?VAR,;).
main(Parameter1 = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 0 + 1 + 2 + 3,
     Parameter2 = 12345678901234567890 + 12345678901234567890,
     Parameter3 = [1, [2, [3, [4, [5, [6, [7, [8, [9, [10, [11, [12]]]]]]]]]]]]
                  ++ [1, [2, [3, [4, [5, [6, [7, [8, [9, [10, [11, [12]]]]]]]]]]]],
     Parameter4 =
         +12345678901234567890 * -12345678901234567890,
     Parameter5 =
         12345678901234567890 >= 12345678901234567890,
     Parameter6 = [1, 2 | 3] ++ [],
     Parameter7 = <<X:4/little-signed-integer-unit:8, Y:4/little-signed-integer-unit:8,>>,
     Parameter8 = ({1234567890123456789012345678901234567890,
                    1234567890123456789012345678901234567890,}),
     Parameter9 = #{foo => 1234567890123456789012345678901234567890, 42 => 1234567890123456789012345678901234567890,}#{42 := 42,},
     Parameter10 = #record.foo,
     Parameter11 = #record{foo = 1234567890123456789012345678901234567890, Var = 1234567890123456789012345678901234567890,},
    ) when 1234567890123456789012345678901234567890,
           1234567890123456789012345678901234567890,;
           1234567890123456789012345678901234567890,
           1234567890123456789012345678901234567890,; ->
    %% line comment 1
    Result1 =
        1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 0 + 1 + 2 + 3,
    Result2 =
        12345678901234567890 + 12345678901234567890,
    Result3 = process ! 11111111111111111111111111,
    Result4 = [1, [2, [3, [4, [5, [6, [7, [8, [9, [10, [11, [12]]]]]]]]]]]]
              ++ [1, [2, [3, [4, [5, [6, [7, [8, [9, [10, [11, [12]]]]]]]]]]]],
    Result5 =
        +12345678901234567890 * -12345678901234567890,
    Result6 =
        12345678901234567890 >= 12345678901234567890,
    Result7 = 12345678901234567890 andalso 12345678901234567890 orelse 12345678901234567890,
    Result8 = catch 1234567890123456789012345678901234567890,
    Result9 = [1, 2 | 3] ++ [],
    Result10 = <<X:4/little-signed-integer-unit:8, Y:4/little-signed-integer-unit:8,>>,
    Result11 = ({1234567890123456789012345678901234567890,
                 1234567890123456789012345678901234567890,}),
    Result12 = begin
                   1234567890123456789012345678901234567890,
                   1234567890123456789012345678901234567890,
               end,
    Result13 = Map#{foo := 1234567890123456789012345678901234567890, 42 := 1234567890123456789012345678901234567890,}#{42 := 42,},
    Result14 = Record#record.foo,
    Result15 = Record#record{foo = 1234567890123456789012345678901234567890, Var = 1234567890123456789012345678901234567890,}#record{foo = 42,},
    Result16 = fun Hoge(1) -> ok,; Hoge(2) -> error,; end,
    Result17 = fun(1) -> ok,; (2) -> error,; end,
    Result18 = {fun foo/0, fun Var1:Var2/Var3,},
    Result19 = [1, 2, [3 / % comment
                       4]],
    Result20 = if
                   1234567890123456789012345678901234567890,
                   1234567890123456789012345678901234567890,;
                   1234567890123456789012345678901234567890,
                   1234567890123456789012345678901234567890,; -> true;

                   true -> true;
                end,
    Result21 = case Result1 of
                   true when 1234567890123456789012345678901234567890,
                             1234567890123456789012345678901234567890,;
                             1234567890123456789012345678901234567890,
                             1234567890123456789012345678901234567890,; -> true,;

                   false -> false,;
               end,
    Result22 = 1 + 1_1 + 001 + 1_6#F_F + 1.1 + 1_1.1_1e1_1,
    receive
        true when 1234567890123456789012345678901234567890,
                  1234567890123456789012345678901234567890,;
                  1234567890123456789012345678901234567890,
                  1234567890123456789012345678901234567890,; -> true,;

        false -> false,;
    end,
    receive
        true when 1234567890123456789012345678901234567890,
                  1234567890123456789012345678901234567890,;
                  1234567890123456789012345678901234567890,
                  1234567890123456789012345678901234567890,; -> true,;

        false -> false,;
    after
        1234567890123456789012345678901234567890 -> true
    end,
    try
        ok,
    of
        ok -> 1234567890123456789012345678901234567890,;
    catch
        Catch1 -> 1234567890123456789012345678901234567890,;

        Catch1:Catch2 -> 1234567890123456789012345678901234567890,;

        Catch1:Catch2:Catch3 -> 1234567890123456789012345678901234567890,;
    end,
    [1234567890123456789012345678901234567890 || X <- [1234567890123456789012345678901234567890], Y <- [1234567890123456789012345678901234567890],],
    <<1234567890123456789012345678901234567890 || <<X>> <= <<1234567890123456789012345678901234567890>>, <<Y>> <= <<1234567890123456789012345678901234567890>>>>,
    _ = foo(),
    [
     %% comment
    ],
    io:format("Lorem ipsum dolor sit amet") % comment 1
    , % comment 2
    ; % comment 3
    . % comment 4
