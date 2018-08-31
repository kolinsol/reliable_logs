-module(logs_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
