-module(pier_tests).
-include_lib("eunit/include/eunit.hrl").

%% runners
pier_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
    {inparallel, [
        fun async_command_subtest/0,
        fun command_subtest/0,
        fun pipeline_subtest/0
    ]}}.

%% tests
async_command_subtest() ->
    {ok, _} = pier:async_command(pier, ["SET", "alice", secret]),
    timer:sleep(500),
    {ok, RequestId} = pier:async_command(pier, ["GET", "alice"]),
    {ok, <<"secret">>} = pier:receive_response(RequestId),

    ok.

command_subtest() ->
    {ok, <<"OK">>} = pier:command(pier, ["SET", "foo", 1]),
    {ok, <<"1">>} = pier:command(pier, ["GET", "foo"]),
    {ok, undefined} = pier:command(pier, ["GET", "foo2"]),
    {ok, 1} = pier:command(pier, ["HSET", "foo3", "key1", "bar"]),
    {ok, <<"bar">>} = pier:command(pier, ["HGET", "foo3", "key1"]),
    {ok, [<<"0">>, Keys]} = pier:command(pier, ["SCAN", 0, "MATCH", "foo*", "COUNT", "1000"]),
    [<<"foo">>, <<"foo3">>] = lists:usort(Keys),
    {error,<<"ERR wrong number of arguments for 'get' command">>} = pier:command(pier, ["GET"]),

    ok.

pipeline_subtest() ->
    {ok, [<<"OK">>, undefined]} = pier:pipeline(pier, [["SET", "hello", "world"], ["GET", "hello2"]]),

    ok.

%% helpers
cleanup(_) ->
    pier_pool:stop(pier),
    pier_app:stop().

setup() ->
    pier_app:start(),
    Ip = os:getenv("REDIS_HOST", "127.0.0.1"),
    pier_pool:start(pier, #{ip => Ip}),
    timer:sleep(500),
    {ok, <<"OK">>} = pier:command(pier, ["FLUSHDB"]).
