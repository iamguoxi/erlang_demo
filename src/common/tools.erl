-module(tools).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-export([
    log/1
    , debug/1
    , dblog/1
    , unixtime/0
    , unixtime/1
    , unixdate/0
    , unixdate/1
    , unixtime_to_now/1
    , longunixtime/0
    , md5/1
    , rand/2
    , ceil/1
    , floor/1
    , string_to_term/1
    , bitstring_to_term/1
    , the_bitstring_to_list/1
    , term_to_string/1
    , term_to_bitstring/1
    , get_ip/1
    , seconds_to_localtime/1
]).

log([A, Mod, Line]) -> log:errlog("ERR : ", "Err Report : ~p~n", A, Mod, Line);
log([F, A, Mod, Line]) -> log:errlog("ERR : ", F, A, Mod, Line).

debug(Info) ->
    ListsNone = [],
    case lists:member(Info, ListsNone) of
        true -> skip;
        false -> log:debug("DEBUG :: ~p~n", [Info])
    end.

dblog([A, Mod, Line]) -> log:dblog("DB : ", "INFO : ~p~n", A, Mod, Line).

unixtime() ->
    {M, S, _} = mod_time:now(),
    M * 1000000 + S.

unixtime(LocalTime) ->
    [UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),
    S1 = calendar:datetime_to_gregorian_seconds(UniversalTime),
    S2 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    S1 - S2.

unixdate() ->
    erlang:now(),
    Now = mod_time:now(),
    {_, Time} = calendar:now_to_local_time(Now),
    Ds = calendar:time_to_seconds(Time),
    {M, S, _} = Now,
    M * 1000000 + S - Ds.

unixdate(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {_, Time} = calendar:now_to_local_time(Now),
    Ds = calendar:time_to_seconds(Time),
    {M, S, _} = Now,
    M * 1000000 + S - Ds.

unixtime_to_now(Time) ->
    M = Time div 1000000,
    S = Time rem 1000000,
    {M, S, 0}.

longunixtime() ->
    {M, S, Ms} = mod_time:now(),
    (M * 1000000000000 + S*1000000 + Ms) div 1000.

ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

md5(S) -> lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    case get("rand_seed") of
        undefined ->
            random:seed(erlang:now()),
            put("rand_seed", 1);
        _ -> skip
    end,
    M = Min - 1,
    if
        Max - M =< 0 -> 0;
        true -> random:uniform(Max - M) + M
    end.

term_to_string(Term) -> binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

term_to_bitstring(Term) -> erlang:list_to_bitstring(io_lib:format("~w", [Term])).

string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error -> undefined
    end.

bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) when is_list(BitString) -> BitString;
bitstring_to_term(BitString) -> string_to_term(binary_to_list(BitString)).

the_bitstring_to_list(undefined) -> "";
the_bitstring_to_list(BitString) when is_list(BitString) -> BitString;
the_bitstring_to_list(BitString) -> binary_to_list(BitString).

get_ip(Socket) ->
    Ip = 
    case inet:peername(Socket) of
        {ok, {Ip0, _Port}} -> Ip0;
        {error, _Reason} -> {0,0,0,0}
    end,
    ip2bin(Ip).
ip2bin({A, B, C, D}) -> [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D)].

seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).