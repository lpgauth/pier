% TODO: improve types

-module(pier).
-include("pier.hrl").

-export([
    async_command/2,
    async_command/3,
    async_command/4,
    command/2,
    command/3,
    pipeline/2,
    pipeline/3,
    receive_response/1
]).

%% public
-spec async_command(atom(), list()) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_command(PoolName, Command) ->
    async_command(PoolName, Command, self()).

-spec async_command(atom(), list(), pid()) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_command(PoolName, Command, Pid) ->
    async_command(PoolName, Command, Pid, ?DEFAULT_TIMEOUT).

-spec async_command(atom(), list(), pid(), pos_integer()) ->
    {ok, shackle:request_id()} | {error, atom()}.

async_command(PoolName, Command, Pid, Timeout) ->
    cast(PoolName, Command, Pid, Timeout).

-spec command(atom(), list()) ->
    {ok, term()} | {error, atom()}.

command(PoolName, Command) ->
    command(PoolName, Command, ?DEFAULT_TIMEOUT).

-spec command(atom(), list(), pos_integer()) ->
    {ok, term()} | {error, atom()}.

command(PoolName, Command, Timeout) ->
    call(PoolName, Command, Timeout).

-spec pipeline(atom(), list()) ->
    {ok, term()} | {error, atom()}.

pipeline(PoolName, Commands) ->
    pipeline(PoolName, Commands, ?DEFAULT_TIMEOUT).

-spec pipeline(atom(), list(), pos_integer()) ->
    {ok, term()} | {error, atom()}.

pipeline(PoolName, Commands, Timeout) ->
    pipeline_call(PoolName, Commands, Timeout).

-spec receive_response(shackle:request_id()) ->
    {ok, term()} | {error, term()}.

receive_response(RequestId) ->
    shackle:receive_response(RequestId).

%% private
call(PoolName, Command, Timeout) ->
    Request = pier_protocol:encode(Command),
    shackle:call(PoolName, Request, Timeout).

cast(PoolName, Command, Pid, Timeout) ->
    Request = pier_protocol:encode(Command),
    shackle:cast(PoolName, Request, Pid, Timeout).

discard_responses([]) ->
    ok;
discard_responses([RequestId | Rest]) ->
    receive_response(RequestId),
    discard_responses(Rest).

pipeline_call(PoolName, Commands, Timeout) ->
    pipeline_call(PoolName, Commands, Timeout, [], []).

pipeline_call(_PoolName, [], _Timeout, [], Responses) ->
    {ok, Responses};
pipeline_call(PoolName, [], Timeout, [RequestId | Rest], Responses) ->
    case receive_response(RequestId) of
        {ok, Response} ->
            pipeline_call(PoolName, [], Timeout, Rest, [Response | Responses]);
        {error, Reason} ->
            discard_responses(Rest),
            {error, Reason}
    end;
pipeline_call(PoolName, [Command | Rest], Timeout, RequestIds, Responses) ->
    Request = pier_protocol:encode(Command),
    case shackle:cast(PoolName, Request, self(), Timeout) of
        {ok, RequestId} ->
            pipeline_call(PoolName, Rest, Timeout, [RequestId | RequestIds], Responses);
        {error, Reason} ->
            {error, Reason}
    end.
