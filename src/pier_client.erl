-module(pier_client).
-include("pier.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    init/1,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer   = <<>>      :: binary(),
    requests = 0         :: non_neg_integer()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init(undefined) ->
    {ok, state()}.

init(_Opts) ->
    {ok, #state {}}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, pos_integer(), binary(), state()}.

handle_request(_Request, #state {
        requests = Requests
    } = State) ->

    RequestId = request_id(Requests),

    {ok, RequestId, <<>>, State#state {
        requests = Requests + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()}.

handle_data(_Data, State) ->
    {ok, [], State}.

-spec terminate(state()) -> ok.

terminate(_State) ->
    ok.

%% private
request_id(N) ->
    (N + 1) rem ?MAX_32_BIT_INT.
