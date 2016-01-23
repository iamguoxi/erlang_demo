-define(ERRLOG(A), tools:log([A, ?MODULE, ?LINE])).
-define(ERRLOG(F, A), tools:log([F, A, ?MODULE, ?LINE])).

-define(DG(Msg), logger:debug(Msg, [], ?MODULE, ?LINE)).
-define(DG(F, A), logger:debug(F, A, ?MODULE, ?LINE)).

-define(INFO(Msg), logger:info(Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), logger:info(F, A, ?MODULE, ?LINE)).

-define(ERR(Msg), logger:error(Msg, [], ?MODULE, ?LINE)).
-define(ERR(F, A), logger:error(F, A, ?MODULE, ?LINE)).