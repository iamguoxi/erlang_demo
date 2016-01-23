-module(hot).
-include("log.hrl").
-define(ETS_FILE_MD5, ets_file_md5).
-record(ets_file_md5, {file_name, md5}).
-export([init_ets/0, hot/0]).

init_ets() ->
    c:cd("../ebin"),
    ets:new(?ETS_FILE_MD5, [public, named_table, {keypos, #ets_file_md5.file_name}]),
    Filemd5List = get_need_update_files(),
    update_file_list_md5(Filemd5List).

hot() ->
    Filemd5List = get_need_update_files(),
    FileList = [File||{File, _Md5}<-Filemd5List],
    hot:u(FileList),
    update_file_list_md5(Filemd5List),
    ok.

get_need_update_files() ->
    {ok, FileList} = file:list_dir("../ebin"),
    F = fun(File, L) ->
        case filename:extension(File) of
            ".beam" -> 
                FileName = filename:rootname(File),
                [erlang:list_to_atom(FileName)|L];
            _ -> L
        end
    end,
    FilterFileList = lists:foldl(F, [], FileList),
    F2 = fun(BeamFileName, HotFileList) ->
        case get_old_file_md5(BeamFileName) of
            none -> 
                NewMd5 = get_file_md5(BeamFileName),
                [{BeamFileName, NewMd5}|HotFileList];
            OldMd5 ->
                NewMd5 = get_file_md5(BeamFileName),
                case OldMd5 == NewMd5 of
                    true -> HotFileList;
                    false -> [{BeamFileName, NewMd5}|HotFileList]
                end
        end
    end,
    lists:foldl(F2, [], FilterFileList).

get_file_md5(BeamFileName) ->
case beam_lib:md5(BeamFileName) of
    {ok, {_, Md5}} -> Md5;
    _Other -> 
        ?DG("~p,~p",[BeamFileName, _Other]),
        0
end.


%% return -> none | Md5
get_old_file_md5(BeamFileName) -> 
    case ets:lookup(?ETS_FILE_MD5, BeamFileName) of
        [] -> none;
        [EtsFileMd5] -> EtsFileMd5#ets_file_md5.md5
    end.

update_file_list_md5(FileList) ->
    F = fun(File) ->
        update_file_md5(File)
    end,
    lists:foreach(F, FileList).

update_file_md5({BeamFileName, Md5}) ->
    EtsFileMd5 = #ets_file_md5{file_name = BeamFileName, md5 = Md5},
    ets:insert(?ETS_FILE_MD5, EtsFileMd5).