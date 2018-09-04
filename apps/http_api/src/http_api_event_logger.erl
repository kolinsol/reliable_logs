-module(http_api_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
                  handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

add_handler() ->
        http_api_event:add_handler(?MODULE, []).

delete_handler() ->
        http_api_event:delete_handler(?MODULE, []).

init([]) ->
        {ok, #state{}}.

handle_call(_Req, State) ->
        Reply = ok,
            {reply, Reply, State}.

handle_info(_Info, State) ->
        {ok, State}.

handle_event({request_received, HandlerName, Method}, State) ->
		error_logger:info_msg("Handler |~p| received |~p| request",
							  [HandlerName, Method]), 
		{ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _New) ->
        {ok, State}.

