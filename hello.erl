-module(hello).
-export([
         main/0, % main
        ]).



%% ハローワールド
main() ->
    io:format(foo, % comment 1
              bar, % comment 2
              ),;.
