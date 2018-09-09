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
    case db_manager_sup:start_link() of
        {ok, Pid} ->
            db_event_logger:add_handler(),
            {ok, Pid};
        Other -> {error, Other}
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
