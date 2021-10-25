% TODO: cleanup
% TODO: support non cluster mode (fallback)
% TODO: stop pools on shutdown

-module(pier_cluster).
-include("pier.hrl").

-export([
    init/0,
    node/1
]).

%% public
-spec init() ->
    ok.

init() ->
    % TODO: add macro for default bootstrap nodes
    [{Ip, Port} | _] = ?GET_ENV(bootstrap_nodes, [{"127.0.0.1", 30001}]),
    {ok, Socket} = connect(Ip, Port),
    Request = pier_protocol:encode(["CLUSTER", "SLOTS"]),
    {ok, HashSlots} = sync_msg(Socket, Request),
    ok = gen_tcp:close(Socket),
    foil:new(?MODULE),
    Nodes = create_hashslot_table(HashSlots, []),
    foil:load(?MODULE),
    start_pools(lists:usort(Nodes)),
    ok.

-spec node(list()) ->
    atom().

node(Key) ->
    % TODO: extract {...}
    HashSlot = ecredis_crc16:crc16(Key) rem ?REDIS_CLUSTER_HASH_SLOTS,
    {ok, PoolName} = foil:lookup(?MODULE, HashSlot),
    PoolName.

%% private
connect(Ip, Port) ->
    SocketOpts = ?DEFAULT_SOCKET_OPTIONS ++ [{active, false}],
    case gen_tcp:connect(Ip, Port, SocketOpts) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

create_hashslot_table([], Acc) ->
    Acc;
create_hashslot_table([[Start, End, [Ip, Port, _] | _] | T], Acc) ->
    PoolName = pool_name(Ip, Port),
    lists:foreach(fun (N) ->
        foil:insert(?MODULE, N, PoolName)
    end, lists:seq(Start, End)),
    create_hashslot_table(T, [{PoolName, binary_to_list(Ip), Port} | Acc]).

pool_name(Ip, Port) ->
    binary_to_atom(<<"pier_", Ip/binary, "_",
        (integer_to_binary(Port))/binary>>, latin1).

rcv_buf(Socket, Buffer) ->
    case gen_tcp:recv(Socket, 0, ?DEFAULT_RECV_TIMEOUT) of
        {ok, Msg} ->
            Buffer2 = <<Buffer/binary, Msg/binary>>,
            case pier_protocol:decode(Buffer2) of
                {ok, Response, <<>>} ->
                    Response;
                {error, not_enough_data} ->
                    rcv_buf(Socket, Buffer2)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

start_pools([]) ->
    ok;
start_pools([{Name, Ip, Port} | T]) ->
    BacklogSize = ?GET_ENV(backlog_size, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?GET_ENV(pool_size, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?GET_ENV(pool_strategy, ?DEFAULT_POOL_STRATEGY),
    Reconnect = ?GET_ENV(reconnect, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?GET_ENV(reconnect_time_max, ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?GET_ENV(reconnect_time_min, ?DEFAULT_RECONNECT_MIN),
    SocketOptions = ?GET_ENV(socket_options, ?DEFAULT_SOCKET_OPTIONS),

    ok = shackle_pool:start(Name, ?CLIENT, [
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
    ]),
    start_pools(T).

sync_msg(Socket, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            rcv_buf(Socket, <<>>);
        {error, Reason} ->
            {error, Reason}
    end.
