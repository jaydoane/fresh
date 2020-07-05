-module(freshener).

-callback refresh(Config) ->
    {ok, Values, ExpiresAfter} | {error, Error} when
        Config :: map(),
        Values :: map(),
        ExpiresAfter :: second(),
        Error :: term().

-behaviour(gen_server).

-export([
    start_link/3,
    value/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(DEFTAULT_REFRESH_EXPIRATION_PERCENT, 90).

-define(DEFAULT_MINIMUM_REFRESH_SEC, 2).

-record(entry, {
    key :: atom(),
    value :: term()
}).

-include("fresh.hrl").


-spec start_link(Module, Name, Config) ->
    {ok, Pid} | {error, Error} | ignore when
        Module :: atom(),
        Name :: atom(),
        Config :: map(),
        Pid :: pid(),
        Error :: term().

start_link(Module, Name, Config) when
        is_atom(Module), is_atom(Name), is_map(Config) ->
    gen_server:start_link({local, Name}, ?MODULE, [Module, Name, Config], []).


-spec value(Name, Key) ->
    {ok, Value} | {error, Error} when
        Name :: atom(),
        Key :: atom(),
        Value :: term(),
        Error :: {missing_ets_key, atom()} | {missing_ets_table, atom()}.

value(Name, Key) ->
    try ets:lookup(Name, Key) of
        [#entry{value = Value}] ->
            {ok, Value};
        [] ->
            {error, {missing_ets_key, Key}}
    catch
        error:badarg ->
            {error, {missing_ets_table, Name}}
    end.


%% gen_server functions


init([Module, Name, Config]) ->
    Opts = [
        named_table,
        {keypos, #entry.key},
        {read_concurrency, true}
    ],
    Name = ets:new(Name, Opts),
    erlang:send_after(0, self(), refresh),
    State = #{
        module => Module,
        name => Name,
        config => Config
    },
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(refresh, State0) ->
    State = refresh(State0),
    NextRefresh = next_refresh(State),
    erlang:send_after(NextRefresh * 1000, self(), refresh),
    {noreply, State#{
        next_refresh => NextRefresh,
        expiration => fresh_time:now(sec) + maps:get(expires_after, State)
    }};

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


%% private


refresh(#{} = State) ->
    #{
        module := Module,
        name := Name,
        config := Config
    } = State,
    case Module:refresh(Config) of
        {ok, Values, ExpiresAfter} ->
            maps:fold(fun(Key, Val, _Acc) ->
                true = ets:insert(Name, #entry{key = Key, value = Val})
            end, [], Values),
            State#{expires_after => ExpiresAfter};
        {error, Error} ->
            %% Retain compatibility with < OTP 21
            error_logger:error_msg("freshener:refresh error ~p", [Error]),
            State#{
                error => #{
                    timestamp => fresh_time:now(sec),
                    reason => Error
                },
                expires_after => 0
            }
    end.


next_refresh(State) ->
    #{
        expires_after := ExpiresAfter,
        config := Config
    } = State,
    ExpirationPercent = maps:get(expiration_percent, Config,
        ?DEFTAULT_REFRESH_EXPIRATION_PERCENT),
    Refresh = trunc(ExpiresAfter * ExpirationPercent / 100),
    MinimumRefresh = maps:get(minimum_refresh_sec, Config,
        ?DEFAULT_MINIMUM_REFRESH_SEC),
    max(MinimumRefresh, Refresh).
