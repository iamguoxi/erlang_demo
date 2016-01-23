-module(gameserver_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1, start_child/2, init/1]).
-define(MAXR, 5).
-define(MAXT, 15).
-define(SHUTDOWN, 300).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> 
    {ok, {{one_for_one, ?MAXR, ?MAXT}, []}}.

start_child(Mod) ->
start_child(Mod, []).

start_child(Mod, Args) ->
    {ok, _} = supervisor:start_child(?MODULE
            , {Mod
            , {Mod, start_link, Args}
            , transient
            , ?SHUTDOWN
            , worker
            , [Mod]
        }),
    ok.