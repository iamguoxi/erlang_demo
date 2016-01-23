-module(gameserver).
-export([start/0, stop/0]).
-define(APPS, [sasl, os_mon, gameserver]).

start()-> 
    ok = start_apps(?APPS).

stop() ->
    ok = stop_apps(?APPS),
    erlang:halt().

start_apps(Apps) ->
    apps_control(
        fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps).

stop_apps(Apps) ->
    apps_control(fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps).

apps_control(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
            case Do(App) of
                ok -> [App | Acc];
                {error, {SkipError, _}} -> Acc;
                {error, Reason} ->
                    lists:foreach(Undo, Acc),
                    throw({error, {ErrorTag, App, Reason}})
            end
        end, [], Apps),
    ok.