-module(hello).
-export([
         main/0 % comment 1
        ]).



%% ハローワールド
main() ->
    %% comment
    io:format("Hello, world!") % comment 1
    , % comment 2
    io:format("Hello, world!") % comment 3
    , % comment 4
    .
