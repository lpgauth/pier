-module(pier).
-include("pier.hrl").

-export([
    query/1,
    async_query/1
]).

%% public

-spec query([term()]) ->
    {ok, term()} | {error, atom()}.

query(Query) ->
    call(Query, ?DEFAULT_TIMEOUT).

-spec async_query([term()]) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_query(Query) ->
    cast(Query, self(), ?DEFAULT_TIMEOUT).

%% private
call(Msg, Timeout) ->
    shackle:call(?APP, Msg, Timeout).

cast(Msg, Pid, Timeout) ->
    shackle:cast(?APP, Msg, Pid, Timeout).
