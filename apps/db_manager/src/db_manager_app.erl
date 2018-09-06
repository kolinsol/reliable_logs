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
    {ok, _P1} = pgapp:connect(insert_pool, [{size, 10},
                                            {database, "reliable_logs"},
                                            {username, "regular"}
                                            ]),
    {ok, _P2} = pgapp:connect(select_pool, [{size, 10},
                                            {database, "reliable_logs"},
                                            {username, "readonly"}
                                            ]),
    db_manager_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
