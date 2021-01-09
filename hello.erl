-module(hello).
-export([main/0]).



%% ハローワールド
main % after main
() % after ()
-> % after ->
    %% line comment
    io:format("Hello, world!").
