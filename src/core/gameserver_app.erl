-module(gameserver_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _) -> 
    [Port] = case init:get_plain_arguments() of
        [Port0] -> [list_to_integer(Port0)];
        _ -> [9999]
    end,
    Log_Level = config:get_config(log_level, []),
    Log_Path = config:get_config(log_path, []),
    [Db_Host, Db_Port, Db_User, Db_Password, Db_Name, Db_Encode] = config:get_config(mysql_config, []),
    {ok, SupPid} = gameserver_sup:start_link(),
    Sup = gameserver_sup,
    sys_report:start(Log_Path, Log_Level),
    ok = start_event(Sup),
    db:start(Db_Host, Db_Port, Db_User, Db_Password, Db_Name, Db_Encode, 20),
    ok = init_ets(),
    ok = start_tcp(Sup, Port),
    ok = start_role(Sup),
    {ok, SupPid}.

stop(_State) ->   
    void.

start_event(Sup) ->
    supervisor:start_child(
        Sup,
        {app_event,
            {app_event, start_link,[]},
            permanent, 10000, supervisor, [app_event]}),
    ok.

init_ets() ->
hot:init_ets(),
ok.

start_tcp(Sup, Port) ->
    {ok,_} = supervisor:start_child(
        Sup,
        {tcp_sup,
            {tcp_sup, start_link, [Port]},
            transient, infinity, supervisor, [tcp_sup]}),
    ok.

start_role(Sup) ->
    {ok,_} = supervisor:start_child(
        Sup,
        {role_connect_sup,
            {role_connect_sup, start_link,[]},
            transient, infinity, supervisor, [role_connect_sup]}),
    ok.