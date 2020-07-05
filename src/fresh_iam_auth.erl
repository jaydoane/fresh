-module(fresh_iam_auth).

-behavior(freshener).

% freshener behaviour callbacks
-export([
    refresh/1
]).

-export([
    start_link/1,
    start_link/2,
    value/1,
    value/2,
    request_token/2,
    request_token/3
]).

-include("fresh.hrl").


-spec start_link(Config) ->
    {ok, Pid} | {error, Error} | ignore when
        Config :: map(),
        Pid :: pid(),
        Error :: term().

% Use only if there will exactly one fresh IAM auth instance per node
start_link(Config) ->
    start_link(?MODULE, Config).


-spec start_link(Name, Config) ->
    {ok, Pid} | {error, Error} | ignore when
        Name :: atom(),
        Config :: map(),
        Pid :: pid(),
        Error :: term().

start_link(Name, Config) ->
    freshener:start_link(?MODULE, Name, Config).


-spec value(Key) ->
    {ok, Value} | {error, Error} when
        Key :: atom(),
        Value :: term(),
        Error :: {missing_ets_key, atom()} | {missing_ets_table, atom()}.

value(Key) ->
    value(?MODULE, Key).


-spec value(Name, Key) ->
    {ok, Value} | {error, Error} when
        Name :: atom(),
        Key :: atom(),
        Value :: term(),
        Error :: {missing_ets_key, atom()} | {missing_ets_table, atom()}.

value(Name, Key) ->
    freshener:value(Name, Key).


-spec refresh(Config) ->
    {ok, Values, ExpiresAfter} | {error, Error} when
        Config :: map(),
        Values :: #{session := binary(), token := binary()},
        ExpiresAfter :: second(),
        Error :: term().

refresh(#{} = Config) ->
    #{
        token_url := TokenUrl,
        token_creds := TokenCreds,
        api_key := ApiKey,
        session_url := SessionUrl
    } = Config,
    case request_token(TokenUrl, ApiKey, TokenCreds) of
        {ok, Token, ExpiresAfter} ->
            ContentType = "application/x-www-form-urlencoded",
            Body = <<"access_token=", Token/binary>>,
            case fresh_session:refresh_cookie(SessionUrl, ContentType, Body) of
                {ok, SessionValues, _SessionExpiresAfter} ->
                    BearerToken = <<"Bearer ", Token/binary>>,
                    Values = SessionValues#{token => BearerToken},
                    {ok, Values, ExpiresAfter};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


request_token(Url, ApiKey) ->
    request_token(Url, ApiKey, "").


-spec request_token(Url, ApiKey, Creds) ->
    {ok, Token, ExpiresAfter} | {error, Error} when
        Url :: string(),
        ApiKey :: string(),
        Creds :: string(),
        Token :: binary(),
        ExpiresAfter :: number(),
        Error :: term().

request_token(Url, ApiKey, Creds) ->
    {ReqHeaders, ReqBody} = headers_body(ApiKey, Creds),
    Request = {Url, ReqHeaders, "", ReqBody},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RspBody}} ->
            #{
                <<"access_token">> := Token,
                <<"expiration">> := Expiration
            } = jiffy:decode(RspBody, [return_maps]),
            {ok, Token, Expiration - fresh_time:now(sec)};
        Error ->
            {error, Error}
    end.


-spec headers_body(ApiKey, Creds) ->
    {Headers, Body} when
        ApiKey :: string(),
        Creds :: string(),
        Headers :: headers(),
        Body :: string().

headers_body(ApiKey, Creds) ->
    Headers = [
        {"Accept", "application/json"},
        {"Content-Type", "application/x-www-form-urlencoded"}
    ] ++ auth_headers(Creds),
    Body = urlencode([
        {"grant_type", "urn:ibm:params:oauth:grant-type:apikey"},
        {"response_type", "cloud_iam"},
        {"apikey", ApiKey}
    ]),
    {Headers, Body}.


-spec auth_headers(Creds) ->
    Headers when
        Creds :: string(),
        Headers :: headers().

auth_headers("") ->
    [];
auth_headers(ColonSeparatedCreds) ->
    B64Auth = base64:encode_to_string(ColonSeparatedCreds),
    [{"Authorization", "Basic " ++ B64Auth}].


-spec urlencode(Props) ->
    Encoded when
        Props :: [{string(), string()}],
        Encoded :: string().

urlencode(Props) ->
    string:join(
        [http_uri:encode(K) ++ "=" ++ http_uri:encode(V) || {K, V} <- Props],
        "&"
    ).
