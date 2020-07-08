-module(fresh_time).

-export([
    now/1,
    wait_value/2,
    wait_value/3
]).

-define(MILLION, 1000000).
-define(MICRO_PER_MILLI, 1000).
-define(DEFAULT_TIMEOUT_MS, 5000).
-define(DEFAULT_DELAY_MS, 50).

-include("fresh.hrl").


-spec now(Unit) ->
    Count when
        Unit :: sec | us,
        Count :: integer().

now(sec) ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * ?MILLION + Sec + round(Micro / ?MILLION);

now(us) ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * ?MILLION + Sec) * ?MILLION + Micro.


-spec wait_value(Name, Key) ->
    {ok, Value} | timeout when
        Name :: atom(),
        Key :: atom(),
        Value :: term().

wait_value(Name, Key) ->
    wait_value(Name, Key, ?DEFAULT_TIMEOUT_MS).


-spec wait_value(Name, Key, TimeoutMs) ->
    {ok, Value} | timeout when
        Name :: atom(),
        Key :: atom(),
        TimeoutMs :: millisecond(),
        Value :: term().

wait_value(Name, Key, TimeoutMs) ->
    wait(fun() ->
        case freshener:value(Name, Key) of
            {ok, Value} ->
                {ok, Value};
            _ ->
                wait
        end
    end, TimeoutMs).


% Lifted from couch/src/test_util.erl

%% wait(Fun) ->
%%     wait(Fun, ?DEFAULT_TIMEOUT_MS).


wait(Fun, TimeoutMs) ->
    wait(Fun, TimeoutMs, ?DEFAULT_DELAY_MS).


wait(Fun, TimeoutMs, DelayMs) ->
    NowUs = now(us),
    wait(Fun, TimeoutMs * ?MICRO_PER_MILLI, DelayMs, NowUs, NowUs).


wait(_Fun, TimeoutUs, _Delay, StartedUs, PrevUs)
        when PrevUs - StartedUs > TimeoutUs ->
    timeout;

wait(Fun, TimeoutUs, DelayMs, StartedUs, _PrevUs) ->
    case Fun() of
        wait ->
            ok = timer:sleep(DelayMs),
            wait(Fun, TimeoutUs, DelayMs, StartedUs, now(us));
        Else ->
            Else
    end.
