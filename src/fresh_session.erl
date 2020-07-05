-module(fresh_session).

-export([
    refresh_cookie/3
]).

-include("fresh.hrl").

-define(DEFAULT_MAX_AGE, "600"). % seconds, from couch_httpd_auth.erl


-spec refresh_cookie(Url, ContentType, Body) ->
    {ok, Values, ExpiresAfter} | {error, Error} when
        Url :: string(),
        ContentType :: string(),
        Body :: iodata(),
        Values :: map(),
        ExpiresAfter :: second(),
        Error :: term().

refresh_cookie(Url, ContentType, Body) ->
    case request_cookie(Url, ContentType, Body) of
        {ok, Cookie} when is_list(Cookie) ->
            Tokens = [string:strip(S) || S <- string:tokens(Cookie, ";")],
            Session = list_to_binary(hd(Tokens)),
            {ok, #{session => Session}, max_age(props(Tokens))};
        {error, Error} ->
            {error, Error}
    end.


request_cookie(Url, ContentType, Body) ->
    Request = {Url, [], ContentType, Body},
    case httpc:request(post, Request, [], []) of
        {ok, {{_, 200, _}, Headers, _}} ->
            {ok, proplists:get_value("set-cookie", Headers)};
        Else ->
            {error, Else}
    end.


props(Tokens) ->
    lists:foldl(fun(Token, Acc) ->
        case string:tokens(Token, "=") of
            [Key, Val] ->
                [{Key, Val} | Acc];
            Else ->
                [Else | Acc]
        end
    end, [], Tokens).


max_age(Props) ->
    list_to_integer(
        proplists:get_value("Max-Age", Props, ?DEFAULT_MAX_AGE)).
