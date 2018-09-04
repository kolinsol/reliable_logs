-module(http_api_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2,
		 request_received/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

request_received(HandlerName, Method) ->
	gen_event:notify(?SERVER, {request_received, HandlerName, Method}).
