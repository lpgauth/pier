-module(pier_pool).
-include("pier.hrl").

-export([
    start/1,
    start/2,
    stop/1
]).

%% public
-spec start(atom()) ->
    ok.

start(PoolName) ->
    start(PoolName, #{}).

-spec start(atom(), map()) ->
    ok.

start(PoolName, Opts) ->
    BacklogSize = maps:get(backlog_size, Opts, ?DEFAULT_BACKLOG_SIZE),
    Ip = maps:get(ip, Opts, ?DEFAULT_IP),
    PoolSize = maps:get(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    PoolStrategy = maps:get(pool_strategy, Opts, ?DEFAULT_POOL_STRATEGY),
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    Reconnect = maps:get(reconnect, Opts, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = maps:get(reconnect_time_max, Opts, ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = maps:get(reconnect_time_min, Opts, ?DEFAULT_RECONNECT_MIN),
    SocketOptions = maps:get(socket_options, Opts, ?DEFAULT_SOCKET_OPTIONS),

    shackle_pool:start(PoolName, ?CLIENT, [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {reconnect_time_max, ReconnectTimeMax},
        {reconnect_time_min, ReconnectTimeMin},
        {socket_options, SocketOptions}
    ], [
        {backlog_size, BacklogSize},
        {pool_size, PoolSize},
        {pool_strategy, PoolStrategy}
    ]).

-spec stop(atom()) ->
    ok.

stop(PoolName) ->
    shackle_pool:stop(PoolName).
