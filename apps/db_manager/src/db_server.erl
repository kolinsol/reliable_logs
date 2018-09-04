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
    {ok, #state{}}.

handle_call({select, Query}, _From, State) ->
    Res = pgapp:squery(select_pool, Query),
    {ok, Columns, Rows} = Res,
    Res2 = transfrom_select_result(Columns, Rows),
    {reply, Res2, State}.

handle_cast({insert, {KeyValue}}, State) ->
    [{<<"log_created">>, LogCreated},
     {<<"app_id">>, AppId},
     {<<"object_id">>, ObjectId},
     {<<"tags">>, Tags},
     {<<"message">>, Message},
     {<<"context">>, Context}] = KeyValue,

    ParsedLogCreated = iso8601:parse_exact(LogCreated),
    EncodedContext = json_processor:encode(Context),
    EncodedTags = json_processor:encode(Tags),

    pgapp:equery(
        insert_pool,
        "insert into logs (log_created, app_id," ++
        "object_id, tags, message, context) VALUES" ++
        "($1, $2, $3, $4, $5, $6);",
        [ParsedLogCreated, AppId, ObjectId,
         EncodedTags, Message, EncodedContext]
    ),

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

transfrom_select_result(Columns, Rows) ->
    ColumnNames = lists:map(
        fun({column, ColumnName, _, _, _, _}) -> ColumnName end,
        Columns
    ),
    Rows2 = lists:map(fun tuple_to_list/1, Rows),
    Rows3 = lists:map(
        fun(Row) -> {lists:zip(ColumnNames, Row)} end,
        Rows2
    ),
    Rows3.
