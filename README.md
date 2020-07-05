# fresh
Maintain valid access tokens and session cookies

## Build

    $ rebar3 compile

## Test

    $ rebar3 do eunit, cover

## Run

```
$ erl -pa ./_build/default/lib/*/ebin -s inets -s ssl
Erlang/OTP 22 [erts-10.6.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Eshell V10.6.1  (abort with ^G)
1> IAMConfig = #{token_url => "https://iam.test.cloud.ibm.com/identity/token", token_creds => "dbcore:XXXXXX", api_key => "XXXXX", session_url => "http://localhost:5984/_iam_session"}.
#{api_key => "XXXXX",
  session_url => "http://localhost:5984/_iam_session",
  token_creds => "dbcore:XXXXX",
  token_url =>
      "https://iam.test.cloud.ibm.com/identity/token"}
2> f(Pid), {ok, Pid} = fresh_iam_auth:start_link(IAMConfig).
{ok,<0.134.0>}
3> fresh_iam_auth:value(token).
{ok,<<"Bearer eyJraWQiOiIyMDIwMDcwMjE2NTMiLCJhbGciOiJSUzI1NiJ9.eyJpYW1faWQiOiJpYW0tU2VydmljZUlkLTY4MjFhMWFkLWUxMDIt"...>>}
4> fresh_iam_auth:value(session).
{ok,<<"IAMSession=aWJtIG9wZW5pZDpzZXJ2aWNlLWlkOmlhbS1TZXJ2aWNlSWQtNjgyMWExYWQtZTEwMi00YWVjLWJkNTQtODYyYWM2MjM2MDk2O"...>>}
```
