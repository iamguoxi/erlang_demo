-module(db).
-include("log.hrl").
-include("mysql.hrl").
-define(DB, mysql_conn).
-export([start/7, exec/1, exec_insertid/1, get/2, transaction/1]).

start(DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, T)->
    mysql:start(?DB, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode),
    lists:foreach(fun(_) ->
            mysql:connect(?DB, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, true)
        end, lists:seq(1, T)),
    ok.

exec(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {updated, R} -> {updated, R#mysql_result.affectedrows};
        {error, R} -> mysql_error([Sql, R#mysql_result.error])
    end.
exec_insertid(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {updated, R} -> {updated, R#mysql_result.insertid};
        {error, R} -> mysql_error([Sql, R#mysql_result.error])
    end.

get(one, Sql) -> get_one_value(Sql);
get(row, Sql) ->
    get_one_row(Sql);
get(all, Sql) ->
    get_all_ans(Sql);
get(R, Sql) -> catch erlang:error({db_error, [R, Sql]}).

%% transaction
transaction(F) ->
    case mysql:transaction(?DB, F) of
        {atomic, R} -> R;
        {updated, R} -> R#mysql_result.affectedrows;
        {error, R} -> mysql_error([R#mysql_result.error]);
        {aborted, {Reason, _}} -> mysql_error([Reason])
    end.

get_all_ans(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, R, _, _, _, _, _}} -> R;
        {error, R} -> mysql_error([Sql, R#mysql_result.error])
    end.

get_one_value(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, [], _, _, _, _, _}} -> [];
        {data, {_, _, [[R]], _, _, _, _, _}} -> R;
        {error, R} -> mysql_error([Sql, R#mysql_result.error])
    end.

get_one_row(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, [], _, _, _, _, _}} -> [];
        {data, {_, _, [R], _, _, _, _, _}} -> R;
        {error, R} -> mysql_error([Sql, R#mysql_result.error]);
        _ -> error
    end.

%% show error log
mysql_error([Sql, Reason]) ->
    catch io:format("mysql error,Sql:~ts~n Reason ~p~n", [Sql,Reason]),
    catch erlang:error({db_error, [Sql, Reason]}).