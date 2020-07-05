-module(fresh_session_cookie).

-behavior(freshener).

% refresher behaviour callbacks
-export([
    refresh/1
]).

-export([
    start_link/1,
    value/1
]).

-include("fresh.hrl").


-spec start_link(Config) ->
    {ok, Pid} | {error, Error} | ignore when
        Config :: map(),
        Pid :: pid(),
        Error :: term().

start_link(Config) ->
    freshener:start_link(?MODULE, ?MODULE, Config).


-spec value(Key) ->
    {ok, Value} | {error, Error} when
        Key :: atom(),
        Value :: term(),
        Error :: {missing_ets_key, atom()} | {missing_ets_table, atom()}.

value(Key) ->
    freshener:value(?MODULE, Key).


-spec refresh(Config) ->
    {ok, Values, ExpiresAfter} | {error, Error} when
        Config :: map(),
        Values :: map(),
        ExpiresAfter :: second(),
        Error :: term().

refresh(#{} = Config) ->
    #{
        session_url := Url,
        username := Username,
        password := Password
    } = Config,
    ContentType = "application/x-www-form-urlencoded",
    Body = "name=" ++ Username ++ "&password=" ++ Password,
    fresh_session:refresh_cookie(Url, ContentType, Body).
