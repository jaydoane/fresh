-module(fresh_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(ACCESS_TOKEN, <<"accesstoken">>).
-define(AUTH_SESSION_COOKIE,
    "AuthSession=YWRtOjVFRUJEMzZFOgfRuxjW7bNeKpWHFCMIVnURuRXh").

-define(IAM_CONFIG, #{
    api_key => "supersecretkey",
    token_url => "https://iam.test.cloud.ibm.com/identity/token",
    token_creds => "secret:password",
    session_url => "http://localhost:5984/_iam_session"
}).


%% All tests have this form:
fixture(Title, Config, SetupFun, TestFuns) ->
    {
        Title,
        {
            setup,
            fun() ->
                {ok, Pid} = SetupFun(Config),
                true = unlink(Pid),
                Pid
            end,
            fun(Pid) ->
            catch exit(Pid, kill),
                meck:unload()
            end,
            fun(_) ->
                TestFuns
            end
        }
    }.

iam_auth_happy_path_test_() ->
    fixture(
        "IAM auth happy path",
        ?IAM_CONFIG,
        fun(Config) ->
            %% note this mock is used for two different requests; one
            %% uses the headers, while the other uses the body
            ok = meck:expect(httpc, request, fun(post, _, _, _) ->
                {ok, {{ok, 200, "OK"},
                    iam_session_cookie_headers(), token_body()}}
            end),
            fresh_iam_auth:start_link(Config)
        end,
        [
            ?_test(begin
                ExpectToken = {ok, <<"Bearer ", ?ACCESS_TOKEN/binary>>},
                ?assertEqual(ExpectToken,
                    fresh_time:wait_value(fresh_iam_auth, token))
            end),
            ?_test(begin
                Expect = {error, {missing_ets_table, non_existent}},
                ?assertEqual(Expect,
                    fresh_iam_auth:value(non_existent, session))
            end)
        ]
    ).


iam_auth_error_test_() ->
    fixture(
        "IAM auth error handling",
        ?IAM_CONFIG,
        fun(Config) ->
            Body = jiffy:encode(#{error => <<"Forbidden">>}),
            ok = meck:expect(httpc, request, fun(post, _, _, _) ->
                {ok, {{ok, 403, "ERROR"}, [], Body}}
            end),
            fresh_iam_auth:start_link(Config)
        end,
        [
            ?_test(begin
                State = sys:get_state(fresh_iam_auth),
                ?assert(maps:get(error, State) /= undefined)
            end)
        ]
    ).


token_body() ->
    jiffy:encode(#{
        access_token => ?ACCESS_TOKEN,
        expiration => fresh_time:now(sec) + 100
    }).


iam_session_cookie_headers() ->
    [{"set-cookie", "IAMSession=aWJtIG9wZW5pZDpzZXJ2aWNlLWlkOmlhbS1TZXJ2aWNlSWQ"
        "tNjgyMWExYWQtZTEwMi00YWVjLWJkNTQtODYyYWM2MjM2MDk2OlFEMDkxQjos799q9AV3X"
        "A7r8yBdEcJL26Ss024OfRjYNQ-4f3OhEA; "
        "Version=1; Expires=Sun, 05-Jul-2020 16:41:35 GMT; Max-Age=3597; "
        "Secure; Path=/; HttpOnly"}].


session_cookie_happy_path_test_() ->
    fixture(
        "Session cookie happy path",
        #{
            username => "username",
            password => "password",
            session_url => "http://localhost:5984/_session"
        },
        fun(Config) ->
            ok = meck:expect(httpc, request, fun(post, _, _, _) ->
                {ok, {{ok, 200, "OK"}, auth_session_cookie_headers(), []}}
            end),
            fresh_session_cookie:start_link(Config)
        end,
        [
            ?_test(begin
                Expect = {ok, list_to_binary(?AUTH_SESSION_COOKIE)},
                ?assertEqual(Expect,
                    fresh_time:wait_value(fresh_session_cookie, session))
            end)
        ]
    ).


auth_session_cookie_headers() ->
    [{"set-cookie", ?AUTH_SESSION_COOKIE
        "; Version=1; Expires=Fri, 19-Jun-2020 20:49:50 GMT; Max-Age=86400; "
        "Path=/; HttpOnly"}].


-endif.
