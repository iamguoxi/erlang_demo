-module(u).
-include_lib("kernel/include/file.hrl").
-include("log.hrl").
-export([c/0, c/1, u/0, u/1, m/1, load/1]).

c() -> c(5).
c(S) when is_integer(S) ->
    c:cd("../ebin"),
    case file:list_dir(".") of
        {ok, FileList} -> 
            Files = get_new_file(FileList, S * 60),
            info("---------check modules---------~n~w~n=========check modules=========", [Files]);
        Any -> info("Error Dir: ~w", [Any])
    end;
c(_) -> info("ERROR======> Badarg", []).

u() -> u(5).
u(m) ->
    StartTime = tools:unixtime(),
    info("----------makes----------", []),
    c:cd("../"),
    make:all(),
    c:cd("ebin"),
    EndTime = tools:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    u(Time / 60);
    u(S) when is_number(S) ->
    c:cd("../ebin"),
    case file:list_dir(".") of
        {ok, FileList} -> 
            Files = get_new_file(FileList, tools:ceil(S * 60) + 3),
            AllZone = [],
            info("---------modules---------~n~w~n----------nodes----------", [Files]),
            load(Files),
            loads(AllZone, Files);
        Any -> info("Error Dir: ~w", [Any])
    end;
u(Files) when is_list(Files) ->
    AllZone = [],
    info("---------modules---------~n~w~n----------nodes----------", [Files]),
    load(Files),
    loads(AllZone, Files);
    u(_) -> info("ERROR======> Badarg", []).

m(Files) when is_list(Files) ->
    StartTime = tools:unixtime(),
    info("----------makes----------~n~w~n", [Files]),
    c:cd("../"),
    Res = make:files(Files, [debug_info,{i, "include"},{outdir, "ebin"}]),
    c:cd("ebin"),
    EndTime = tools:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    Res.

info(V, P) -> io:format(V ++ "~n", P).

%% 更新到所有线路
loads([], _Files) -> ok;
loads([Node | T], Files) ->
    info("[~w]", [Node]),
    rpc:cast(Node, u, load, [Files]),
    loads(T, Files).

get_new_file(Files, S) -> get_new_file(Files, S, []).
get_new_file([], _S, Result) -> Result;
get_new_file([H | T], S, Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
            case file:read_file_info(H) of
                {ok, FileInfo} -> 
                Now = calendar:local_time(),
                case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                    {Days, Times} -> 
                        Seconds = calendar:time_to_seconds(Times), 
                        case Days =:= 0 andalso Seconds < S of
                            true ->
                                FileName = list_to_atom(Left),
                                [FileName | Result];
                            false -> Result
                        end;
                    _ -> Result
                end;
                _ -> Result
            end;
        _ -> Result
    end,
    get_new_file(T, S, NewResult).

load([]) -> ok;
load([FileName | T]) ->
    c:l(FileName),
    info("loaded: ~w", [FileName]),
    load(T).