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
    buffer       = <<>> :: binary(),
    pattern             :: binary:cp(),
    requests_in  = 0    :: non_neg_integer(),
    requests_out = 0    :: non_neg_integer()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init(undefined) ->
    {ok, state()}.

init(_Opts) ->
    {ok, #state {
        pattern = binary:compile_pattern(<<"\r\n">>)
    }}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, pos_integer(), binary(), state()}.

handle_request(Request, #state {
        requests_out = RequestsOut
    } = State) ->

    {ok, RequestsOut + 1, Request, State#state {
        requests_out = RequestsOut + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()}.

handle_data(Data, #state {
        buffer = Buffer,
        pattern = Pattern,
        requests_in = RequestsIn
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    case pier_protocol:decode(Data2, Pattern) of
        {error, not_enough_data} ->
            {ok, [], State#state {
                buffer = Data2
            }};
        {ok, Response, Buffer2} ->
            {ok, [{RequestsIn + 1, Response}], State#state {
                buffer = Buffer2,
                requests_in = RequestsIn + 1
            }}
    end.

-spec terminate(state()) -> ok.

terminate(_State) ->
    ok.
