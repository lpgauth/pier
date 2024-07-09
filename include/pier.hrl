%% macros
-define(APP, pier).
-define(CLIENT, pier_client).

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_PORT, 6379).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, timer:minutes(2)).
-define(DEFAULT_RECONNECT_MIN, timer:seconds(1)).
-define(DEFAULT_RECV_TIMEOUT, 1000).
-define(DEFAULT_SOCKET_OPTIONS, [
    binary,
    {buffer, 65535},
    {nodelay, true},
    {packet, raw},
    {send_timeout, 50},
    {send_timeout_close, true}
]).
-define(DEFAULT_TIMEOUT, 1000).
