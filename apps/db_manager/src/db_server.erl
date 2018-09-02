%%%-------------------------------------------------------------------
%%% @author kolinsol
%%%-------------------------------------------------------------------
-module(db_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    pgapp:connect(save_pool, [{size, 10},
                              {database, "reliable_logs"},
                              {username, "kolinsol"}
                              ]),
    pgapp:connect(select_pool, [{size, 10},
                              {database, "reliable_logs"},
                              {username, "kolinsol"}
                              ]),
    Res = pgapp:squery(save_pool, "select * from logs"),
    io:format("~p~n", [Res]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

