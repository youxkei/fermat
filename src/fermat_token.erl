-module(fermat_token).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([tokenize/1, token/1, token/2, category/1, text/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Types
%%----------------------------------------------------------------------------------------------------------------------
-export_type([token/0, category/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-type category() :: atom(). % TODO: enumerate categories

-record(?MODULE, {category :: category(),
                  text :: string(),
                  line :: non_neg_integer() | undefined,
                  column :: pos_integer()| undefined}).

-type token() :: #?MODULE{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc tokenize the source code
-spec tokenize(string()) -> {ok, [fermat_token:token()]} | {error, term()}.
tokenize(SourceCode) ->
    case erl_scan:string(SourceCode, 1, [return, text]) of
        {ok, ErlScanTokens, _} -> {ok, from_erl_scan_tokens(ErlScanTokens)};
        {error, _, _} = Error -> Error
    end.

-spec token(category()) -> token().
token(Category) -> #?MODULE{category = Category,
                            text = "",
                            line = undefined,
                            column = undefined}.

-spec token(category(), string()) -> token().
token(Category, Text) -> #?MODULE{category = Category,
                                  text = Text,
                                  line = undefined,
                                  column = undefined}.

-spec category(Token :: token()) -> category().
category(#?MODULE{category = Category}) -> Category.

-spec text(Token :: token()) -> string().
text(#?MODULE{text = Text}) -> Text.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec from_erl_scan_tokens(ErlScanTokens :: [erl_scan:token()]) -> [token()].
from_erl_scan_tokens(ErlScanTokens) ->
    lists:map(fun(ErlScanToken) ->
                  Category = erl_scan:category(ErlScanToken),
                  case Category of
                      dot -> #?MODULE{category = dot,
                                      text = ".", % remove trailing line break"
                                      line = erl_scan:line(ErlScanToken),
                                      column = erl_scan:column(ErlScanToken)};
                      _ -> #?MODULE{category = Category,
                                    text = erl_scan:text(ErlScanToken),
                                    line = erl_scan:line(ErlScanToken),
                                    column = erl_scan:column(ErlScanToken)}
                  end
              end, ErlScanTokens).
