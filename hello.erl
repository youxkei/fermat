-module(hello).
-export([
         main/0, % comment 1
        ]).



%% ハローワールド
main() ->
    %% line comment 1
    Result =
        catch io:format("Lorem ipsum dolor sit amet").
