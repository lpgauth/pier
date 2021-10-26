-module(pier).
-include("pier.hrl").

-export([
    query/1,
    query/2,
    async_query/1
]).

%% public
-spec query([term()]) ->
    {ok, term()} | {error, atom()}.

query(Query) ->
    call(Query, ?DEFAULT_TIMEOUT).

-spec query([term()], pos_integer()) ->
    {ok, term()} | {error, atom()}.

query(Query, Timeout) ->
    call(Query, Timeout).

-spec async_query([term()]) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_query(Query) ->
    cast(Query, self(), ?DEFAULT_TIMEOUT).

%% private
call(Request, Timeout) ->
    shackle:call(?APP, pier_protocol:encode(Request), Timeout).

cast(Request, Pid, Timeout) ->
    shackle:cast(?APP, pier_protocol:encode(Request), Pid, Timeout).
