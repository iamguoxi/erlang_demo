-module(tcp_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Port) ->
    supervisor:start_link(tcp_sup, {10, Port}).

init({AcceptorCount, Port}) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    tcp_acceptor_sup,
                        {tcp_acceptor_sup, start_link, []},
                        transient,
                        infinity,
                        supervisor,
                        [tcp_acceptor_sup]
                },
                {
                    lists:concat([tcp_listener_,Port]),
                    {tcp_listener, start_link, [AcceptorCount, Port]},
                        transient,
                        100,
                        worker,
                        [tcp_listener]
                }
            ]
        }
    }.