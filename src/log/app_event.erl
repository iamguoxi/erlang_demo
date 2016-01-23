-module(app_event).
-export([start_link/0, add_handler/2, del_handler/2, lookup/1, create/2, replace/2, delete/1]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

del_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

lookup(Key) ->
    gen_event:notify(?MODULE, {lookup, Key}).

create(Key, Value) ->
    gen_event:notify(?MODULE, {create, {Key, Value}}).

replace(Key, Value) ->
    gen_event:notify(?MODULE, {replace, {Key, Value}}).

delete(Key) ->
    gen_event:notify(?MODULE, {delete, Key}).