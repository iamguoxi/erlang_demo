-module(config).
-export([get_config/3, get_config/2, get_tcp_set/0]).

get_config(Config, Type, Default) ->
    case application:get_env(Config, Type) of
        {ok, Res} -> Res;
        _ -> Default
    end.

get_config(Type, Default) ->
    case application:get_env(gameserver, Type) of
        {ok, Res} -> Res;
        _ -> Default
    end.

get_tcp_set() ->
    [binary
    , {packet, 0}
    , {active, false}
    , {reuseaddr, true}
    , {nodelay, false}
    , {delay_send, true}
    , {send_timeout, 5000}
    , {keepalive, true}
    , {exit_on_close, true}].