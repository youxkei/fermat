-module(hello).
-export([
         % comment 1
         main/0

         , % comment 2
         %% comment 3
         % comment 4
         % comment 5
         main/0, main/0, main/0, main/0
         % comment 6
        ]).



%% ハローワールド
main() ->
    %% comment
    io:format("Hello, world!")
    %% comment 8
    . % comment 7
