-module(hello).
-export([

         main/0, % comment
         %% comment
         % comment
         main/0, main/0, main/0, main/0

        ]).



%% ハローワールド
main() ->
    %% comment
    io:format("Hello, world!").
