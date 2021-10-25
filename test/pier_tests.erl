-module(pier_tests).
-include_lib("eunit/include/eunit.hrl").

%% runners
pier_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
    {inparallel, [
        fun query_subtest/0
    ]}}.

%% tests
query_subtest() ->
    {ok, <<"OK">>} = pier:query(["SET", "foo", "bar"]),
    ok.

%% helpers
cleanup(_) ->
    pier_app:stop().

setup() ->
    pier_app:start().
