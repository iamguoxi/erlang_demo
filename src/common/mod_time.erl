-module(mod_time).
-behaviour(gen_server).
-export([start/1, now/0]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(INTERVAL, 100).

start(Sup) ->
    supervisor:start_child(Sup, 
        {?MODULE,
            {?MODULE, start_link, []},
            permanent, brutal_kill, worker, [?MODULE]}).

now() -> 
    [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
    Now.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new (ets_timer, [set, protected, named_table ,{read_concurrency,true}]),
    ets:insert(ets_timer, {timer, {erlang:now(), 0}}),
    erlang:send_after(?INTERVAL, self(), {event, clock}),
    {ok, []}.

handle_call(_Request, _From, State) -> {reply, State, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({event, syn_all, Time}, State) ->
    ets:insert(ets_timer, {timer, {Time, 0}}),
    {noreply, State};
    handle_info({event, clock}, State) ->
    ets:insert(ets_timer, {timer, {erlang:now(), 0}}),
    erlang:send_after(?INTERVAL, self(), {event, clock}),
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.