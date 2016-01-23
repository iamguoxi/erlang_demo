-module(mysql_auth).
-export([do_old_auth/6,do_new_auth/7]).
-define(LONG_PASSWORD, 1).
-define(LONG_FLAG, 4).
-define(PROTOCOL_41, 512).
-define(TRANSACTIONS, 8192).
-define(SECURE_CONNECTION, 32768).
-define(CONNECT_WITH_DB, 8).
-define(MAX_PACKET_SIZE, 1000000).
-define(SELFMODULE, mysql).
-define(ERRLOG(A), tools:dblog([A, ?SELFMODULE, ?LINE])).

do_old_auth(Sock, RecvPid, SeqNum, User, Password, Salt1) ->
    Auth = password_old(Password, Salt1),
    Packet2 = make_auth(User, Auth),
    do_send(Sock, Packet2, SeqNum),
    mysql_conn:do_recv(RecvPid, SeqNum).

do_new_auth(Sock, RecvPid, SeqNum, User, Password, Salt1, Salt2) ->
    Auth = password_new(Password, Salt1 ++ Salt2),
    Packet2 = make_new_auth(User, Auth, none),
    do_send(Sock, Packet2, SeqNum),
    case mysql_conn:do_recv(RecvPid, SeqNum) of
        {ok, Packet3, SeqNum2} ->
            case Packet3 of
                <<254:8>> ->
                    AuthOld = password_old(Password, Salt1),
                    do_send(Sock, <<AuthOld/binary, 0:8>>, SeqNum2 + 1),
                    mysql_conn:do_recv(RecvPid, SeqNum2 + 1);
                _ -> {ok, Packet3, SeqNum2}
            end;
        {error, Reason} -> {error, Reason}
    end.

password_old(Password, Salt) ->
    {P1, P2} = hash(Password),
    {S1, S2} = hash(Salt),
    Seed1 = P1 bxor S1,
    Seed2 = P2 bxor S2,
    List = rnd(9, Seed1, Seed2),
    {L, [Extra]} = lists:split(8, List),
    list_to_binary(lists:map(fun (E) -> E bxor (Extra - 64) end, L)).

%% part of do_old_auth/4, which is part of mysql_init/4
make_auth(User, Password) ->
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS,
    Maxsize = 0,
    UserB = list_to_binary(User),
    PasswordB = Password,
    <<Caps:16/little, Maxsize:24/little, UserB/binary, 0:8,
    PasswordB/binary>>.

%% part of do_new_auth/4, which is part of mysql_init/4
make_new_auth(User, Password, Database) ->
    DBCaps = case Database of
        none -> 0;
        _ -> ?CONNECT_WITH_DB
    end,
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor
    ?PROTOCOL_41 bor ?SECURE_CONNECTION bor DBCaps,
    Maxsize = ?MAX_PACKET_SIZE,
    UserB = list_to_binary(User),
    PasswordL = size(Password),
    DatabaseB = case Database of
        none -> <<>>;
        _ -> list_to_binary(Database)
    end,
    <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8,
    UserB/binary, 0:8, PasswordL:8, Password/binary, DatabaseB/binary>>.

hash(S) -> hash(S, 1345345333, 305419889, 7).

hash([C | S], N1, N2, Add) ->
    N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
    N2_1 = N2 + ((N2 * 256) bxor N1_1),
    Add_1 = Add + C,
    hash(S, N1_1, N2_1, Add_1);
    hash([], N1, N2, _Add) ->
    Mask = (1 bsl 31) - 1,
    {N1 band Mask , N2 band Mask}.

rnd(N, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).

rnd(0, List, _, _) -> lists:reverse(List);
rnd(N, List, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
    NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
    Float = (float(NSeed1) / float(Mod))*31,
    Val = trunc(Float)+64,
    rnd(N - 1, [Val | List], NSeed1, NSeed2).



dualmap(_F, [], []) -> [];
dualmap(F, [E1 | R1], [E2 | R2]) -> [F(E1, E2) | dualmap(F, R1, R2)].

bxor_binary(B1, B2) -> list_to_binary(dualmap(fun (E1, E2) -> E1 bxor E2 end, binary_to_list(B1), binary_to_list(B2))).

password_new([], _Salt) -> <<>>;
password_new(Password, Salt) ->
    Stage1 = crypto:hash(sha, Password),
    Stage2 = crypto:hash(sha, Stage1),
    {sha, Data1} = crypto:hash_init(sha),
    {sha, Data2} = crypto:hash_update({sha, Data1}, Salt),
    {sha, Data3} = crypto:hash_update({sha, Data2}, Stage2),
    Res = crypto:hash_final({sha, Data3}),
    bxor_binary(Res, Stage1).


do_send(Sock, Packet, Num) ->
    Data = <<(size(Packet)):24/little, Num:8, Packet/binary>>,
    gen_tcp:send(Sock, Data).