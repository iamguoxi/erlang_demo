-module(mysql_conn).
-export([start/7, start_link/7, fetch/3, fetch/4, execute/5, execute/6, transaction/3, transaction/4]).
-export([fetch_local/2, execute_local/3, get_pool_id/1 ]).
-export([do_recv/2]).
-include("mysql.hrl").
-record(state, {
    mysql_version,
    recv_pid,
    socket,
    data,
    %% maps statement names to their versions
    prepares = gb_trees:empty(),
    %% the id of the connection pool to which this connection belongs
    pool_id
}).
-define(SECURE_CONNECTION, 32768).
-define(MYSQL_QUERY_OP, 3).
-define(DEFAULT_STANDALONE_TIMEOUT, 5000).
-define(MYSQL_4_0, 40). %% Support for MySQL 4.0.x
-define(MYSQL_4_1, 41). %% Support for MySQL 4.1.x et 5.0.x
%% Used by transactions to get the state variable for this connection when bypassing the dispatcher.
-define(STATE_VAR, mysql_connection_state). 
-define(SELFMODULE, mysql).
-define(ERRLOG(A), tools:dblog([A, ?SELFMODULE, ?LINE])).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(Host, Port, User, Password, Database)
%% Function: start_link(Host, Port, User, Password, Database)
%%           Host     = string()
%%           Port     = integer()
%%           User     = string()
%%           Password = string()
%%           Database = string()
%% Descrip.: Starts a mysql_conn process that connects to a MySQL
%%           server, logs in and chooses a database.
%% Returns : {ok, Pid} | {error, Reason}
%%           Pid    = pid()
%%           Reason = string()
%%--------------------------------------------------------------------
start(Host, Port, User, Password, Database, Encoding, PoolId) ->
    ConnPid = self(),
    Pid = spawn(fun() -> init(Host, Port, User, Password, Database, Encoding, PoolId, ConnPid) end),
    post_start(Pid).

start_link(Host, Port, User, Password, Database, Encoding, PoolId) ->
    ConnPid = self(),
    Pid = spawn_link(fun() -> init(Host, Port, User, Password, Database, Encoding, PoolId, ConnPid) end),
    post_start(Pid).

%% part of start/6 or start_link/6:
post_start(Pid) ->
    receive
        {mysql_conn, Pid, ok} -> {ok, Pid};
        {mysql_conn, Pid, {error, Reason}} -> {error, Reason};
        {mysql_conn, OtherPid, {error, Reason}} ->
            ?ERRLOG(["Ignoring message from [OtherPid, Reason] => ", [OtherPid, Reason]]),
            post_start(Pid);
        Unknown ->
            ?ERRLOG(["Ignoring message from [Unknown] => ", [Unknown]]),
            post_start(Pid)
    after 5000 -> {error, "timed out"}
    end.

%%--------------------------------------------------------------------
%% Function: fetch(Pid, Query, From)
%%           fetch(Pid, Query, From, Timeout)
%%           Pid     = pid(), mysql_conn to send fetch-request to
%%           Queries   = A single binary() query or a list of binary() queries.
%%                     If a list is provided, the return value is the return
%%                     of the last query, or the first query that has
%%                     returned an error. If an error occurs, execution of
%%                     the following queries is aborted.
%%           From    = pid() or term(), use a From of self() when
%%                     using this module for a single connection,
%%                     or pass the gen_server:call/3 From argument if
%%                     using a gen_server to do the querys (e.g. the
%%                     mysql_dispatcher)
%%           Timeout = integer() | infinity, gen_server timeout value
%% Descrip.: Send a query or a list of queries and wait for the result
%%           if running stand-alone (From = self()), but don't block
%%           the caller if we are not running stand-alone
%%           (From = gen_server From).
%% Returns : ok                        | (non-stand-alone mode)
%%           {data, #mysql_result}     | (stand-alone mode)
%%           {updated, #mysql_result}  | (stand-alone mode)
%%           {error, #mysql_result}      (stand-alone mode)
%%           FieldInfo = term()
%%           Rows      = list() of [string()]
%%           Reason    = term()
%%--------------------------------------------------------------------
fetch(Pid, Queries, From) -> fetch(Pid, Queries, From, ?DEFAULT_STANDALONE_TIMEOUT).

fetch(Pid, Queries, From, Timeout) -> do_fetch(Pid, Queries, From, Timeout).

execute(Pid, Name, Version, Params, From) -> execute(Pid, Name, Version, Params, From, ?DEFAULT_STANDALONE_TIMEOUT).

execute(Pid, Name, Version, Params, From, Timeout) -> send_msg(Pid, {execute, Name, Version, Params, From}, From, Timeout).

transaction(Pid, Fun, From) -> transaction(Pid, Fun, From, ?DEFAULT_STANDALONE_TIMEOUT).

transaction(Pid, Fun, From, Timeout) -> send_msg(Pid, {transaction, Fun, From}, From, Timeout).

get_pool_id(State) -> State#state.pool_id.

%%====================================================================
%% Internal functions
%%====================================================================

fetch_local(State, Query) -> do_query(State, Query).

execute_local(State, Name, Params) ->
    case do_execute(State, Name, Params, undefined) of
        {ok, Res, State1} ->
            put(?STATE_VAR, State1),
            Res;
        Err -> Err
    end.

do_recv(RecvPid, SeqNum) when SeqNum == undefined ->
    receive
        {mysql_recv, RecvPid, data, Packet, Num} ->
            {ok, Packet, Num};
        {mysql_recv, RecvPid, closed, E} ->
            ?ERRLOG(["mysql_recv: socket was closed", E]),
            {error, io_lib:format("mysql_recv: socket was closed ~p", [E])}
    end;
do_recv(RecvPid, SeqNum) when is_integer(SeqNum) ->
    ResponseNum = SeqNum + 1,
    receive
        {mysql_recv, RecvPid, data, Packet, ResponseNum} ->
            {ok, Packet, ResponseNum};
        {mysql_recv, RecvPid, closed, E} ->
            ?ERRLOG(["mysql_recv: socket was closed", E]),
            {error, io_lib:format("mysql_recv: socket was closed ~p", [E])}
    end.

do_fetch(Pid, Queries, From, Timeout) -> send_msg(Pid, {fetch, Queries, From}, From, Timeout).

send_msg(Pid, Msg, From, Timeout) ->
    Self = self(),
    Pid ! Msg,
    case From of
        Self ->
            receive {fetch_result, Pid, Result} -> Result
                after Timeout -> {error, "message timed out"}
            end;
        _ -> ok
    end.

init(Host, Port, User, Password, Database, Encoding, PoolId, Parent) ->
    case mysql_recv:start_link(Host, Port, self()) of
        {ok, RecvPid, Sock} ->
            case mysql_init(Sock, RecvPid, User, Password) of
                {ok, Version} ->
                    Db = iolist_to_binary(Database),
                    case do_query(Sock, RecvPid, <<"use `", Db/binary, "`">>, Version) of
                        {error, MySQLRes} ->
                            ?ERRLOG(["mysql_conn: Failed changing to database: [Database, Res] => ",
                            [Database, mysql:get_result_reason(MySQLRes)]]),
                            Parent ! {mysql_conn, self(),
                            {error, failed_changing_database}};
                        {_ResultType, _MySQLRes} ->
                            Parent ! {mysql_conn, self(), ok},
                            case Encoding of
                                undefined -> undefined;
                                _ ->
                                    EncodingBinary = list_to_binary(atom_to_list(Encoding)),
                                    do_query(Sock, RecvPid, <<"set names '", EncodingBinary/binary, "'">>, Version)
                            end,
                            State = #state{mysql_version=Version,
                                recv_pid = RecvPid,
                                socket   = Sock,
                                pool_id  = PoolId,
                                data     = <<>>
                            },
                            process_flag(priority, high),
                            loop(State)
                    end;
                {error, _Reason} -> Parent ! {mysql_conn, self(), {error, login_failed}}
            end;
        E ->
            ?ERRLOG(["failed connecting to: [Host, Port, E] => ", [Host, Port, E]]),
            Parent ! {mysql_conn, self(), {error, connect_failed}}
    end.

%%--------------------------------------------------------------------
%% Function: loop(State)
%%           State = state record()
%% Descrip.: Wait for signals asking us to perform a MySQL query, or
%%           signals that the socket was closed.
%% Returns : error | does not return
%%--------------------------------------------------------------------
loop(State) ->
    RecvPid = State#state.recv_pid,
    receive
        {fetch, Queries, From} ->
            send_reply(From, do_queries(State, Queries)),
            loop(State);
        {transaction, Fun, From} ->
            put(?STATE_VAR, State),
            Res = do_transaction(State, Fun),
            %% The transaction may have changed the state of this process
            %% if it has executed prepared statements. This would happen in
            %% mysql:execute.
            State1 = get(?STATE_VAR),
            send_reply(From, Res),
            loop(State1);
        {execute, Name, Version, Params, From} ->
            State1 =
            case do_execute(State, Name, Params, Version) of
                {error, _} = Err ->
                    send_reply(From, Err),
                    State;
                {ok, Result, NewState} ->
                    send_reply(From, Result),
                    NewState
            end,
            loop(State1);
        {mysql_recv, RecvPid, data, Packet, Num} ->
            ?ERRLOG(["received data when not expecting any ignoring it : [Num, Packet] => ", [Num, Packet]]),
            loop(State);
        Unknown ->
            ?ERRLOG(["received unknown signal, exiting : [Unknown] => ", [Unknown]]),
            error
    end.

%% GenSrvFrom is either a gen_server:call/3 From term(),
%% or a pid if no gen_server was used to make the query
send_reply(GenSrvFrom, Res) when is_pid(GenSrvFrom) -> GenSrvFrom ! {fetch_result, self(), Res};
send_reply(GenSrvFrom, Res) -> gen_server:reply(GenSrvFrom, Res).

do_query(State, Query) -> do_query(State#state.socket, State#state.recv_pid, Query, State#state.mysql_version).

do_query(Sock, RecvPid, Query, Version) ->
    Query1 = iolist_to_binary(Query),
    Packet =  <<?MYSQL_QUERY_OP, Query1/binary>>,
    case do_send(Sock, Packet, 0) of
        ok -> get_query_response(RecvPid, Version);
        {error, Reason} ->
            Msg = io_lib:format("Failed sending data on socket : ~p", [Reason]),
            ?ERRLOG([Msg]),
            {error, Msg}
    end.

do_queries(State, Queries) when not is_list(Queries) -> do_query(State, Queries);
do_queries(State, Queries) -> do_queries(State#state.socket, State#state.recv_pid, Queries, State#state.mysql_version).

%% Execute a list of queries, returning the response for the last query.
%% If a query returns an error before the last query is executed, the
%% loop is aborted and the error is returned. 
do_queries(Sock, RecvPid, Queries, Version) ->
    catch lists:foldl(
        fun(Query, _LastResponse) ->
            case do_query(Sock, RecvPid, Query, Version) of
                {error, _} = Err -> throw(Err);
                Res -> Res
            end
        end, ok, Queries).

do_transaction(State, Fun) ->
    case do_query(State, <<"BEGIN">>) of
        {error, _} = Err -> {aborted, Err};
        _ ->
            case catch Fun() of
                error = Err -> rollback(State, Err);
                {error, _} = Err -> rollback(State, Err);
                {'EXIT', _} = Err -> rollback(State, Err);
                Res ->
                case do_query(State, <<"COMMIT">>) of
                    {error, _} = Err -> rollback(State, {commit_error, Err});
                    _ ->
                        case Res of
                            {atomic, _} -> Res;
                            _ -> {atomic, Res}
                        end
                end
            end
    end.

rollback(State, Err) ->
    Res = do_query(State, <<"ROLLBACK">>),
    {aborted, {Err, {rollback_result, Res}}}.

do_execute(State, Name, Params, ExpectedVersion) ->
    Res = case gb_trees:lookup(Name, State#state.prepares) of
        {value, Version} when Version == ExpectedVersion -> {ok, latest};
        {value, Version} -> mysql:get_prepared(Name, Version);
        none -> mysql:get_prepared(Name)
    end,
    case Res of
        {ok, latest} -> {ok, do_execute1(State, Name, Params), State};
        {ok, {Stmt, NewVersion}} -> prepare_and_exec(State, Name, NewVersion, Stmt, Params);
        {error, _} = Err -> Err
    end.

prepare_and_exec(State, Name, Version, Stmt, Params) ->
    NameBin = atom_to_binary(Name),
    StmtBin = <<"PREPARE ", NameBin/binary, " FROM '",
    Stmt/binary, "'">>,
    case do_query(State, StmtBin) of
        {updated, _} ->
            State1 =
            State#state{
            prepares = gb_trees:enter(Name, Version, State#state.prepares)},
            {ok, do_execute1(State1, Name, Params), State1};
        {error, _} = Err -> Err;
            Other -> {error, {unexpected_result, Other}}
    end.

do_execute1(State, Name, Params) ->
    Stmts = make_statements_for_execute(Name, Params),
    do_queries(State, Stmts).

make_statements_for_execute(Name, []) ->
    NameBin = atom_to_binary(Name),
    [<<"EXECUTE ", NameBin/binary>>];
make_statements_for_execute(Name, Params) ->
    NumParams = length(Params),
    ParamNums = lists:seq(1, NumParams),
    NameBin = atom_to_binary(Name),
    ParamNames =
        lists:foldl(
            fun(Num, Acc) ->
                    ParamName = [$@ | integer_to_list(Num)],
                    if 
                        Num == 1 -> ParamName ++ Acc;
                        true -> [$, | ParamName] ++ Acc
                    end
                end, [], lists:reverse(ParamNums)),
    ParamNamesBin = list_to_binary(ParamNames),
    ExecStmt = <<"EXECUTE ", NameBin/binary, " USING ", ParamNamesBin/binary>>,
    ParamVals = lists:zip(ParamNums, Params),
    Stmts = lists:foldl(
        fun({Num, Val}, Acc) ->
                NumBin = mysql:encode(Num, true),
                ValBin = mysql:encode(Val, true),
                [<<"SET @", NumBin/binary, "=", ValBin/binary>> | Acc]
            end, [ExecStmt], lists:reverse(ParamVals)),
    Stmts.

atom_to_binary(Val) ->
    <<_:4/binary, Bin/binary>> = term_to_binary(Val),
    Bin.

%%--------------------------------------------------------------------
%% Function: mysql_init(Sock, RecvPid, User, Password)
%%           Sock     = term(), gen_tcp socket
%%           RecvPid  = pid(), mysql_recv process
%%           User     = string()
%%           Password = string()
%% Descrip.: Try to authenticate on our new socket.
%% Returns : ok | {error, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
mysql_init(Sock, RecvPid, User, Password) ->
    case do_recv(RecvPid, undefined) of
        {ok, Packet, InitSeqNum} ->
            {Version, Salt1, Salt2, Caps} = greeting(Packet),
            AuthRes = case Caps band ?SECURE_CONNECTION of
                ?SECURE_CONNECTION -> mysql_auth:do_new_auth(Sock, RecvPid, InitSeqNum + 1, User, Password, Salt1, Salt2);
                _ -> mysql_auth:do_old_auth(Sock, RecvPid, InitSeqNum + 1, User, Password, Salt1)
            end,
            case AuthRes of
                {ok, <<0:8, _Rest/binary>>, _RecvNum} -> {ok,Version};
                {ok, <<255:8, Rest/binary>>, _RecvNum} ->
                    {Code, ErrData} = get_error_data(Rest, Version),
                    ?ERRLOG(["init error: [Code, ErrData]", [Code, ErrData]]),
                    {error, ErrData};
                {ok, RecvPacket, _RecvNum} ->
                    ?ERRLOG(["init unknown error: [RecvPacket]", [binary_to_list(RecvPacket)]]),
                    {error, binary_to_list(RecvPacket)};
                {error, Reason} ->
                    ?ERRLOG(["init failed receiving data: [Reason]", [Reason]]),
                    {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% part of mysql_init/4
greeting(Packet) ->
    <<_Protocol:8, Rest/binary>> = Packet,
    {Version, Rest2} = asciz(Rest),
    <<_TreadID:32/little, Rest3/binary>> = Rest2,
    {Salt, Rest4} = asciz(Rest3),
    <<Caps:16/little, Rest5/binary>> = Rest4,
    <<_ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, _Rest7} = asciz(Rest6),
    {normalize_version(Version), Salt, Salt2, Caps}.

%% part of greeting/2
asciz(Data) when is_binary(Data) -> mysql:asciz_binary(Data, []);
asciz(Data) when is_list(Data) ->
    {String, [0 | Rest]} = lists:splitwith(fun (C) ->
    C /= 0
    end, Data),
    {String, Rest}.

get_query_response(RecvPid, Version) ->
    case do_recv(RecvPid, undefined) of
        {ok, Packet, _} ->
            {Fieldcount, Rest} = get_lcb(Packet),
            case Fieldcount of
                0 ->%% No Tabular data
                    {AffectedRows, Rest2} = get_lcb(Rest),
                    {InsertId, _} = get_lcb(Rest2),
                    {updated, #mysql_result{affectedrows=AffectedRows, insertid=InsertId}};
                255 ->
                    case get_error_data(Rest, Version) of
                        {Code, {SqlState, Message}} ->   
                        % MYSQL_4_1 error data
                        {error, #mysql_result{error=Message, 
                        errcode=Code,
                        errsqlstate=SqlState}};
                        {Code, Message} -> 
                        % MYSQL_4_0 error data
                        {error, #mysql_result{error=Message,
                        errcode=Code}}
                    end;
                _ ->
                    %% Tabular data received
                    case get_fields(RecvPid, [], Version) of
                        {ok, Fields} ->
                            case get_rows(Fields, RecvPid, [], Version) of
                                {ok, Rows} -> {data, #mysql_result{fieldinfo=Fields, rows=Rows}};
                                {error, {Code, {SqlState, Message}}} ->   {error, #mysql_result{error=Message, errcode=Code, errsqlstate=SqlState}};
                                {error, {Code, Message}} -> {error, #mysql_result{error=Message, errcode=Code}}
                            end;
                        {error, Reason} -> {error, #mysql_result{error=Reason}}
                    end
            end;
        {error, Reason} -> {error, #mysql_result{error=Reason}}
    end.

%% Support for MySQL 4.0.x:
get_fields(RecvPid, Res, ?MYSQL_4_0) ->
    case do_recv(RecvPid, undefined) of
        {ok, Packet, _Num} -> case Packet of
            <<254:8>> -> {ok, lists:reverse(Res)};
            <<254:8, Rest/binary>> when size(Rest) < 8 -> {ok, lists:reverse(Res)};
            _ ->
                {Table, Rest} = get_with_length(Packet),
                {Field, Rest2} = get_with_length(Rest),
                {LengthB, Rest3} = get_with_length(Rest2),
                LengthL = size(LengthB) * 8,
                <<Length:LengthL/little>> = LengthB,
                {Type, Rest4} = get_with_length(Rest3),
                {_Flags, _Rest5} = get_with_length(Rest4),
                This = {Table,
                Field,
                Length,
                Type},
                get_fields(RecvPid, [This | Res], ?MYSQL_4_0)
            end;
        {error, Reason} -> {error, Reason}
    end;
%% Support for MySQL 4.1.x and 5.x:
get_fields(RecvPid, Res, ?MYSQL_4_1) ->
    case do_recv(RecvPid, undefined) of
        {ok, Packet, _Num} -> case Packet of
            <<254:8>> -> {ok, lists:reverse(Res)};
            <<254:8, Rest/binary>> when size(Rest) < 8 -> {ok, lists:reverse(Res)};
            _ ->
                {_Catalog, Rest} = get_with_length(Packet),
                {_Database, Rest2} = get_with_length(Rest),
                {Table, Rest3} = get_with_length(Rest2),
                %% OrgTable is the real table name if Table is an alias
                {_OrgTable, Rest4} = get_with_length(Rest3),
                {Field, Rest5} = get_with_length(Rest4),
                %% OrgField is the real field name if Field is an alias
                {_OrgField, Rest6} = get_with_length(Rest5),
                <<_Metadata:8/little, _Charset:16/little,
                Length:32/little, Type:8/little,
                _Flags:16/little, _Decimals:8/little,
                _Rest7/binary>> = Rest6,
                This = {Table, Field, Length, get_field_datatype(Type)},
                get_fields(RecvPid, [This | Res], ?MYSQL_4_1)
            end;
        {error, Reason} -> {error, Reason}
    end.

get_rows(Fields, RecvPid, Res, Version) ->
    case do_recv(RecvPid, undefined) of
        {ok, Packet, _Num} ->
        case Packet of
            <<254:8, Rest/binary>> when size(Rest) < 8 -> {ok, lists:reverse(Res)};
            <<255:8, Rest/binary>> ->
                {Code, ErrData} = get_error_data(Rest, Version),            
            {error, {Code, ErrData}};
            _ ->
                {ok, This} = get_row(Fields, Packet, []),
                get_rows(Fields, RecvPid, [This | Res], Version)
        end;
        {error, Reason} -> {error, Reason}
    end.

%% part of get_rows/4
get_row([], _Data, Res) -> {ok, lists:reverse(Res)};
get_row([Field | OtherFields], Data, Res) ->
    {Col, Rest} = get_with_length(Data),
    This = case Col of
        null ->
            undefined;
        _ ->
            convert_type(Col, element(4, Field))
    end,
get_row(OtherFields, Rest, [This | Res]).

get_with_length(Bin) when is_binary(Bin) ->
    {Length, Rest} = get_lcb(Bin),
    case get_lcb(Bin) of 
        {null, Rest} -> {null, Rest};
        _ -> split_binary(Rest, Length)
    end.

get_lcb(<<251:8, Rest/binary>>) -> {null, Rest};
get_lcb(<<252:8, Value:16/little, Rest/binary>>) -> {Value, Rest};
get_lcb(<<253:8, Value:24/little, Rest/binary>>) -> {Value, Rest};
get_lcb(<<254:8, Value:32/little, Rest/binary>>) -> {Value, Rest};
get_lcb(<<Value:8, Rest/binary>>) when Value < 251 -> {Value, Rest};
get_lcb(<<255:8, Rest/binary>>) -> {255, Rest}.

do_send(Sock, Packet, SeqNum) when is_binary(Packet), is_integer(SeqNum) ->
    Data = <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>,
    gen_tcp:send(Sock, Data).

normalize_version([$4,$.,$0|_T]) ->
    ?ERRLOG(["switching to MySQL 4.0.x protocol."]),
    ?MYSQL_4_0;
normalize_version([$4,$.,$1|_T]) -> ?MYSQL_4_1;
normalize_version([$5|_T]) -> ?MYSQL_4_1; 
normalize_version(_Other) ->
    ?ERRLOG(["MySQL version not supported: MySQL Erlang module might not work correctly."]),
    ?MYSQL_4_0.

%%--------------------------------------------------------------------
% Function: get_field_datatype(DataType)
%%           DataType = integer(), MySQL datatype
%% Descrip.: Return MySQL field datatype as description string
%% Returns : String, MySQL datatype
%%--------------------------------------------------------------------
get_field_datatype(0) ->   'DECIMAL';
get_field_datatype(1) ->   'TINY';
get_field_datatype(2) ->   'SHORT';
get_field_datatype(3) ->   'LONG';
get_field_datatype(4) ->   'FLOAT';
get_field_datatype(5) ->   'DOUBLE';
get_field_datatype(6) ->   'NULL';
get_field_datatype(7) ->   'TIMESTAMP';
get_field_datatype(8) ->   'LONGLONG';
get_field_datatype(9) ->   'INT24';
get_field_datatype(10) ->  'DATE';
get_field_datatype(11) ->  'TIME';
get_field_datatype(12) ->  'DATETIME';
get_field_datatype(13) ->  'YEAR';
get_field_datatype(14) ->  'NEWDATE';
get_field_datatype(246) -> 'NEWDECIMAL';
get_field_datatype(247) -> 'ENUM';
get_field_datatype(248) -> 'SET';
get_field_datatype(249) -> 'TINYBLOB';
get_field_datatype(250) -> 'MEDIUM_BLOG';
get_field_datatype(251) -> 'LONG_BLOG';
get_field_datatype(252) -> 'BLOB';
get_field_datatype(253) -> 'VAR_STRING';
get_field_datatype(254) -> 'STRING';
get_field_datatype(255) -> 'GEOMETRY'.

convert_type(Val, ColType) ->
    case ColType of
        T when T == 'TINY';
        T == 'SHORT';
        T == 'LONG';
        T == 'LONGLONG';
        T == 'INT24';
        T == 'YEAR' -> list_to_integer(binary_to_list(Val));
        T when T == 'TIMESTAMP';
        T == 'DATETIME' ->
            {ok, [Year, Month, Day, Hour, Minute, Second], _Leftovers} =
            io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Val)),
            {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
        'TIME' ->
            {ok, [Hour, Minute, Second], _Leftovers} =
            io_lib:fread("~d:~d:~d", binary_to_list(Val)),
            {time, {Hour, Minute, Second}};
        'DATE' ->
            {ok, [Year, Month, Day], _Leftovers} =
            io_lib:fread("~d-~d-~d", binary_to_list(Val)),
            {date, {Year, Month, Day}};
            T when T == 'DECIMAL';
            T == 'NEWDECIMAL';
            T == 'FLOAT';
        T == 'DOUBLE' ->
            {ok, [Num], _Leftovers} =
            case io_lib:fread("~f", binary_to_list(Val)) of
                {error, _} -> io_lib:fread("~d", binary_to_list(Val));
                Res -> Res
            end,
            Num;
        _Other -> Val
    end.

get_error_data(ErrPacket, ?MYSQL_4_0) ->
    <<Code:16/little, Message/binary>> = ErrPacket,
    {Code, binary_to_list(Message)};
get_error_data(ErrPacket, ?MYSQL_4_1) ->
    <<Code:16/little, _M:8, SqlState:5/binary, Message/binary>> = ErrPacket,
    {Code, {binary_to_list(SqlState), binary_to_list(Message)}}.