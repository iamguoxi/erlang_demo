-module(sys_report).
-behaviour(gen_event).
-export([start/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {fd, file, loglv}).

start(LogPath, Loglv) ->
    error_logger:add_report_handler(sys_report, [LogPath, Loglv]).

init([LogPath, Loglv]) ->
    {{Y, M, D},_} = erlang:localtime(),
    Node = add_node(self()),
    File = LogPath ++ "/log_" ++ integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D) ++ "/" ++ Node ++ ".log",
    filelib:ensure_dir(File),
    case file:open(File, [write, append]) of
        {ok, Fd} -> {ok, #state{fd = Fd, file = File, loglv = Loglv}};
        Error -> Error
    end.

handle_event(Event, State) ->
    write_event(State#state.fd, State#state.loglv, Event),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info({'EXIT', _Fd, _Reason}, _State) ->
    remove_handler;
handle_info({emulator, _GL, reopen}, State) ->
    file:close(State#state.fd),
    case file:open(State#state.file, [append, raw]) of
        {ok, Fd} ->
            {ok, State#state{fd = Fd}};
        Error ->
            Error
    end;
handle_info({emulator, _GL, _Chars}, State) ->
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write_event(Fd, Loglv, {error, _Gleader, {Pid, Format, Data}}) when Loglv >= 2 ->
    write(Fd, ["ERROR", [{Pid, Format, Data}]]);
write_event(Fd, Loglv, {error_report, _Gleader, {Pid, Type, Report}})  when Loglv >= 3 ->
    write(Fd, ["ERROR_REPORT", [{Pid, Type, Report}]]);
write_event(Fd, Loglv, {warning_msg, _Gleader, {Pid, Format, Data}})  when Loglv >= 4 ->
    write(Fd, ["WARNING_MSG", [{Pid, Format, Data}]]);
write_event(Fd, Loglv, {warning_report, _Gleader, {Pid, Type, Report}})  when Loglv >= 5 ->
    write(Fd, ["WARNING_REPORT", [{Pid, Type, Report}]]);
write_event(Fd, Loglv, {info_msg, _Gleader, {Pid, Format, Data}})  when Loglv >= 6 ->
    write(Fd, ["INFO_MSG", [{Pid, Format, Data}]]);
write_event(Fd, Loglv, {info_report, _Gleader, {Pid, Type, Report}}) when Loglv >= 7  ->
    write(Fd, ["INFO_REPORT", [{Pid, Type, Report}]]);
write_event(_Fd, _Loglv, _Info) ->
    ok.

write(IoF, [Title, A]) ->
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Format = list_to_binary(Title ++ "  ~s\r\n~p~n\r\n~n"),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(IoF, unicode:characters_to_list(Format), [Date] ++ A). 

add_node(Pid) when is_pid(Pid) ->
    {A, _} = lists:splitwith(fun(N) -> N /= 64 end, lists:concat(["_",node(Pid)])),
    A;
add_node(_) ->
    "NO NODE INFO".