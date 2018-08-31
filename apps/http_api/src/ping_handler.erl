-module(ping_handler).

-export([init/3]).

init(_Type, Req, Opts) ->
    {ok, Resp} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
	], <<"pong">>, Req),
	{ok, Resp, Opts}.
