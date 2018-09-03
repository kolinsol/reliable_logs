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
    Res = pgapp:squery(save_pool, "select * from logs"),
    io:format("~p~n", [Res]),
    {ok, #state{}}.

handle_call({select, Query}, _From, State) ->
    Res = pgapp:squery(select_pool, Query),
    io:format("select ~p~n", [Res]),
    {ok, _Columns, Rows} = Res,
    {reply, Rows, State}.

handle_cast({insert, JSON}, State) ->
    Res = pgapp:equery(
        save_pool,
        "insert into logs (log_created, created, app_id," ++
        "object_id, tags, message, context) VALUES" ++
        "($1, $2, $3, $4, $5, $6, $7);",
        [{{2000, 10, 10}, {12, 2, 2}},
         {{2000, 10, 10}, {12, 2, 2}},
         <<"s1">>, 1212, 1, <<"ss">>, 2]
    ),
    io:format("insert ~p~n", [Res]),
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

