-module(fermat).
-compile([nowarn_unused_vars]).
-compile([nowarn_unused_function]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([main/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(ARG_SPECS, [
                    {help, $h, "help", undefined, "Show CLI usage"}
                   ]).

-type option_map() :: #{help => boolean(),
                        files => [file:filename()]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% escript Entry point
-spec main([string()]) -> no_return().
main(Args) ->
    #{files := Files} = parse_args(Args),
    lists:foreach(fun(File) ->
                          {ok, SourceCode} = file:read_file(File),
                          {ok, Tokens} = fermat_token:tokenize(binary_to_list(SourceCode)),
                          {ok, LayoutExpr} = fermat_layout_expr:parse([Token || Token <- Tokens, fermat_token:category(Token) =/= white_space andalso fermat_token:category(Token) =/= comment]),
                          RightMargin = 80,
                          FormattedLayoutExpr = fermat_formatter:format(LayoutExpr, RightMargin, 1, 1),
                          io:format("~s:~n", [File]),
                          print_layout_expr(FormattedLayoutExpr, 0),
                          io:format("~n~n"),
                          ok
                  end,
                  Files),
    erlang:halt(0).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc parse commandline arguments
-spec parse_args([string()]) -> option_map().
parse_args(Args) ->
    case getopt:parse(?ARG_SPECS, Args) of
        {ok, {Options, Globs}} ->
            NormalizedOptions = normalize_options(Options),
            OptionMap = maps:from_list(NormalizedOptions),

            case OptionMap of
                #{help := true} ->
                    ok = show_usage(),
                    erlang:halt(0);
                _ when length(Globs) > 0 ->
                    OptionMap#{files => lists:flatmap(fun(Glob) -> filelib:wildcard(Glob) end, Globs)};
                _ ->
                    io:format(standard_error, "Error: missing arguments~n", []),
                    ok = show_usage(),
                    erlang:halt(1)
            end;
        {error, {Reason, Data}} ->
            io:format(standard_error, "Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(?ARG_SPECS, escript:script_name()),
            erlang:halt(1)
    end.

%% @doc [help, {tokenizer, "erl_scan"}] -> [{help, true}, {tokenizer, "erl_scan"}]
-spec normalize_options([atom() | {atom(), any()}]) -> [{atom(), any()}].
normalize_options(Options) ->
    lists:map(fun(Option) ->
                      case Option of
                          Option when is_atom(Option) -> {Option, true};
                          _ ->
                              io:format(standard_error, "Error: invalid commandline option: ~p~n", [Option]),
                              erlang:halt(0)
                      end
              end,
              Options).

%% @doc show the CLI usage
-spec show_usage() -> ok.
show_usage() -> ok = getopt:usage(?ARG_SPECS, escript:script_name(), "<file/glob ...>").

print_layout_expr(empty, _) -> 0;
print_layout_expr(after_arrow, _) -> io:format(" "), 1;
print_layout_expr({text, Text}, _) -> io:format("~s", [Text]), length(Text);
print_layout_expr({juxtaposition, Lhs, Rhs}, Indent) -> LhsSpan = print_layout_expr(Lhs, Indent),
                                                        LhsSpan + print_layout_expr(Rhs, Indent + LhsSpan);
print_layout_expr({stacking, Lhs, empty}, Indent) -> print_layout_expr(Lhs, Indent);
print_layout_expr({stacking, Lhs, Rhs}, Indent) -> print_layout_expr(Lhs, Indent),
                                                   io:format("\n"),
                                                   print_indent(Indent), Indent + print_layout_expr(Rhs, Indent);
print_layout_expr({choice, Lhs, _Rhs}, Indent) -> print_layout_expr(Lhs, Indent);
print_layout_expr({indent, LayoutExpr}, Indent) -> io:format("    "), 4 + print_layout_expr(LayoutExpr, Indent + 4);
print_layout_expr({wrap, LayoutExprs}, Indent) -> lists:foldl(fun({1, LayoutExpr}, Span) -> Span + print_layout_expr(LayoutExpr, Indent + Span);
                                                                 ({_, LayoutExpr}, Span) -> io:format(" "), 1 + Span + print_layout_expr(LayoutExpr, Indent + 1 + Span)
                                                              end, 0, lists:zip(lists:seq(1, length(LayoutExprs)), LayoutExprs)).

print_indent(0) -> ok;
print_indent(N) -> io:format(" "), print_indent(N - 1).
