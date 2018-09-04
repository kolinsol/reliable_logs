-module(ping_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    {Method, _TempReq} = cowboy_req:method(Req),
    http_api_event:request_received(?MODULE, Method),

    {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
	], <<"{\"success\": true}">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
