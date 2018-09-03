%%%-------------------------------------------------------------------
%% @doc db_manager public API
%%%-------------------------------------------------------------------

-module(db_manager_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    pgapp:connect(insert_pool, [{size, 10},
                              {database, "reliable_logs"},
                              {username, "kolinsol"}
                              ]),
    pgapp:connect(select_pool, [{size, 10},
                              {database, "reliable_logs"},
                              {username, "kolinsol"}
                              ]),
    db_manager_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
