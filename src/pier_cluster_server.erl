-module(pier_cluster_server).

-export([
    start_link/0
]).

%% metal callbacks
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {}).

-type state() :: #state {}.

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    metal:start_link(?MODULE, ?MODULE, undefined).

%% metal callbacks
-spec init(atom(), pid(), undefined) ->
    no_return().

init(_Name, _Parent, undefined) ->
    {ok, #state {}}.

-spec handle_msg(term(), state()) ->
    {ok, state()}.

handle_msg(_Msg, State) ->
    {ok, State}.

-spec terminate(term(), state()) ->
    ok.

terminate(_Reason, _State) ->
    ok.
