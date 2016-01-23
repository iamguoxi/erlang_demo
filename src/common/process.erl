-module(process).
-export([whereis/2, register/3, unregister/2, is_process_alive/1]).

%% @return pid() | port() | undefined
whereis(local, NameAtom) -> 
    erlang:whereis(NameAtom);
whereis(global, NameTerm) ->
    global:whereis_name(NameTerm).

register(local, NameAtom, PidOrPort) ->
    erlang:register(NameAtom, PidOrPort),
    ok;
register(global, NameTerm, PidOrPort) ->
    global:re_register_name(NameTerm, PidOrPort),
    ok.

unregister(local, NameAtom) ->
    erlang:unregister(NameAtom),
    ok;
unregister(global, NameTerm) ->
    global:unregister_name(NameTerm),
    ok.

%% 检查进程是否存活 
is_process_alive(Pid) ->    
    try 
        if 
            is_pid(Pid) ->
                case node(Pid) =:= node() of
                    true -> erlang:is_process_alive(Pid);
                    false -> 
                        case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                            {badrpc, _Reason}  -> false;
                            Res -> Res
                        end
                end;
            true -> false
        end
    catch 
        _:_ -> false
    end.