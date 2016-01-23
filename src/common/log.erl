-module(log).
-compile(export_all).

log_utf8(Msg) when is_list(Msg)->
    {ok, S} = file:open("log_utf8.txt", append),
    [DescList] = io_lib:format("~ts", [Msg]),   
    DescBin = erlang:iolist_to_binary(DescList),   
    DescList2 = unicode:characters_to_list(DescBin),   
    _Bin = unicode:characters_to_binary(DescList2),
    file:close(S).

debug(F, A) ->
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    F3 = case get("debug") of
        undefined ->
            LogPath = config:get_config(log_path, []),
            {Node, _} = lists:splitwith(fun(N) -> N /= 64 end, lists:concat(["_",node()])),
            File1 = LogPath ++  "/debug_" ++ integer_to_list(M) ++ integer_to_list(D) ++ Node ++ ".txt",
            {ok, Fl} = file:open(File1, [write, append]),
            put("debug", Fl),
            Fl;
        F2 -> F2
    end,
    Format = list_to_binary("#debug" ++ " ~s \n" ++ F ++ "\n"),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":",   integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Date] ++ [A]). 

errlog(F, A) ->
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    F3 = case get("errlog") of
        undefined ->
            LogPath = config:get_config(log_path, []),
            {Node, _} = lists:splitwith(fun(N) -> N /= 64 end, lists:concat(["",node()])),
            File1 = LogPath ++  "/log_" ++ integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D) ++ Node ++ ".log",
            {ok, Fl} = file:open(File1, [write, append]),
            put("errlog", Fl),
            Fl;
        F2 -> F2
    end,
    Format = list_to_binary("#error" ++ " [~s] || \n" ++ F ++ "\n\n"),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":",   integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Date] ++ [A]).  

errlog(T, F, A, Mod, Line) ->
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    F3 = case get("errlog") of
        undefined ->
            LogPath = config:get_config(log_path, []),
            {Node, _} = lists:splitwith(fun(N) -> N /= 64 end, lists:concat(["",node()])),
            File1 = LogPath ++  "/log_" ++ integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D) ++ Node ++ ".log",
            {ok, Fl} = file:open(File1, [write, append]),
            put("errlog", Fl),
            Fl;
        F2 -> F2
    end,
    Format = list_to_binary("#" ++ T ++ "[ ~w ] [ ~w ] [ ~s ] \n " ++ F ++ "\n\n"),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":",   integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Mod, Line, Date] ++ [A]).

dblog(T, F, A, Mod, Line) ->
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    F3 = case get("dblog") of
        undefined ->
            LogPath = config:get_config(log_path, []),
            {Node, _} = lists:splitwith(fun(N) -> N /= 64 end, lists:concat(["",node()])),
            File1 = LogPath ++  "/db_" ++ integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D) ++ Node ++ ".log",
            {ok, Fl} = file:open(File1, [write, append]),
            put("dblog", Fl),
            Fl;
        F2 -> F2
    end,
    Format = list_to_binary("#" ++ T ++ "[ ~w ] [ ~w ] [ ~s ] \n " ++ F ++ "\n\n"),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":",   integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Mod, Line, Date] ++ [A]).