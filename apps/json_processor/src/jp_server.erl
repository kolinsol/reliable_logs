%%%-------------------------------------------------------------------
%%% @author kolinsol
%%%
%%% Created : 2018-09-01 17:42:16.614014
%%%-------------------------------------------------------------------
-module(jp_server).

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

-record(state, {schema}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, File} = file:read_file("/Users/kolinsol/Documents/dev/erlang/wg-test/reliable_logs/apps/json_processor/schemas/insert_schema.json"),
    Decoded = jiffy:decode(File),
    {ok, #state{schema = Decoded}}.

handle_call({decode, JSON}, _From, State) ->
    Decoded = jiffy:decode(JSON),
    {reply, Decoded, State};
handle_call({validate, JSON, _SchemaName}, _From, State) ->
    Result = jesse:validate_with_schema(State#state.schema, JSON),
    {reply, Result, State};
handle_call({encode, JSON}, _From, State) ->
    Encoded = jiffy:encode(JSON),
    {reply, Encoded, State}.

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




