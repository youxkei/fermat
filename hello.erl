-module(hello).
-export([
         main/0, % comment 1
        ]).



%% ハローワールド
main() ->
    %% line comment 1
    io:format("Hello, world!") % comment 3
    , % comment 4
    ; % comment 5
    .
