-module(role_connect).
-define(TCP_TIMEOUT, 1000).
-define(HEART_TIMEOUT, 60 * 1000).
-define(HEADER_LENGTH, 2).
-export([start_link/0, init/0]).

start_link() ->
 	{ok, proc_lib:spawn_link(?MODULE, init, [])}.

init() ->
    process_flag(trap_exit, true),
    receive 
  		{start, Socket} ->
       		handle_info(Socket);
		_ ->
			skip
    end.

handle_info(Socket) ->
    case async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT) of
		{error} -> gen_tcp:close(Socket);
		_ -> 
		    receive
		        {inet_async, Socket, _, {ok, <<Short:16, Bin/binary>>}} ->
                    io:format("(~p)recv: ~p~n", [Socket, {Short, Bin}]),
                    handle_info(Socket);
		        {inet_async, Socket, _, {error, timeout}} ->
		            gen_tcp:close(Socket);
		        _ ->
		            gen_tcp:close(Socket)
		    end
	end.

async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, _Reason} -> {error};
        {ok, Res}        ->  Res; 
        Res              ->	Res
    end.
