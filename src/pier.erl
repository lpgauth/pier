% TODO: improve types

-module(pier).
-include("pier.hrl").

-export([
    query/1,
    query/2,
    async_query/1,
    async_query/2,
    async_query/3
]).

%% public
-spec query(list()) ->
    {ok, term()} | {error, atom()}.

query(Query) ->
    query(Query, ?DEFAULT_TIMEOUT).

-spec query(list(), pos_integer()) ->
    {ok, term()} | {error, atom()}.

query(Query, Timeout) ->
    call(Query, Timeout).

-spec async_query(list()) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_query(Query) ->
    async_query(Query, self(), ?DEFAULT_TIMEOUT).

-spec async_query(list(), pid()) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_query(Query, Pid) ->
    async_query(Query, Pid, ?DEFAULT_TIMEOUT).

-spec async_query(list(), pid(), pos_integer()) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_query(Query, Pid, Timeout) ->
    cast(Query, Pid, Timeout).

%% private
call([_Command, Key | _] = Query, Timeout) ->
    Request = pier_protocol:encode(Query),
    Pool = pier_cluster:node(Key),
    shackle:call(Pool, Request, Timeout).

cast([_Command, Key | _] = Query, Pid, Timeout) ->
    Request = pier_protocol:encode(Query),
    Pool = pier_cluster:node(Key),
    shackle:cast(Pool, Request, Pid, Timeout).
