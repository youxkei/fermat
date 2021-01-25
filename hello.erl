%%                                               |
-module(hello).
-export([
         main/0, % comment 1
        ]).



%% ハローワールド
main() ->
    %% line comment 1
    Result1 =
        1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 0,
    Result2 =
        12345678901234567890 + 12345678901234567890,
    Result3 = process ! 11111111111111111111111111,
    io:format("Lorem ipsum dolor sit amet").
