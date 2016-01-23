-module(mysql_recv).
-export([start_link/3]).

-record(state, {
    socket,
    parent,
    data
}).
-define(SECURE_CONNECTION, 32768).
-define(CONNECT_TIMEOUT, 5000).
-define(SELFMODULE, mysql).
-define(ERRLOG(A), tools:dblog([A, ?SELFMODULE, ?LINE])).
start_link(Host, Port, Parent) when is_list(Host), is_integer(Port) ->
    RecvPid = spawn_link(fun () -> init(Host, Port, Parent) end),
    %% wait for the socket from the spawned pid
    receive
        {mysql_recv, RecvPid, init, {error, E}} -> {error, E};
        {mysql_recv, RecvPid, init, {ok, Socket}} -> {ok, RecvPid, Socket}
    after ?CONNECT_TIMEOUT ->
        catch exit(RecvPid, kill),
        {error, "timeout"}
    end.

init(Host, Port, Parent) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
        {ok, Sock} ->
            Parent ! {mysql_recv, self(), init, {ok, Sock}},
            State = #state{socket  = Sock,
                parent  = Parent,
                data    = <<>>
            },
            loop(State);
        E ->
            ?ERRLOG(["mysql_recv: Failed connecting to: [Host, Port, E] => ", [Host, Port, E]]),
            Msg = lists:flatten(io_lib:format("connect failed : ~p", [E])),
            Parent ! {mysql_recv, self(), init, {error, Msg}}
    end.

loop(State) ->
    Sock = State#state.socket,
    receive
        {tcp, Sock, InData} ->
            NewData = list_to_binary([State#state.data, InData]),
            Rest = sendpacket(State#state.parent, NewData),
            loop(State#state{data = Rest});
        {tcp_error, Sock, Reason} ->
            ?ERRLOG(["mysql_recv: Socket closed: [Sock, Reason] => ", [Sock, Reason]]),
            State#state.parent ! {mysql_recv, self(), closed, {error, Reason}},
            error;
        {tcp_closed, Sock} ->
            ?ERRLOG(["mysql_recv: Socket closed: [Sock] => ", [Sock]]),
            State#state.parent ! {mysql_recv, self(), closed, normal},
            error
    end.

%% send data to parent if we have enough data
sendpacket(Parent, Data) ->
    case Data of
        <<Length:24/little, Num:8, D/binary>> ->
            if
                Length =< size(D) ->
                    {Packet, Rest} = split_binary(D, Length),
                    Parent ! {mysql_recv, self(), data, Packet, Num},
                    sendpacket(Parent, Rest);
                true -> Data
            end;
        _ -> Data
    end.