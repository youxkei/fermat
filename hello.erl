-module(hello).
-export([main/0]).



%% ハローワールド
main % after main
() % after ()
-> % after ->
    io:format("Hello, world!").
