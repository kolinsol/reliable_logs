-module(db_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

add_handler() ->
        db_event:add_handler(?MODULE, []).

delete_handler() ->
        db_event:delete_handler(?MODULE, []).

init([]) ->
        {ok, #state{}}.

handle_call(_Req, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_info(_Info, State) ->
        {ok, State}.

handle_event({success, OpName}, State) ->
		error_logger:info_msg("Operation |~p| succeeded~n", [OpName]),
		{ok, State};
handle_event({failure, OpName, Err}, State) ->
		error_logger:info_msg("Operation |~p| failed with error:~n~p~n",
                              [OpName, Err]),
		{ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _New) ->
        {ok, State}.
