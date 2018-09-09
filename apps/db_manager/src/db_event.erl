-module(db_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         success/1,
         failure/2
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

success(OpName) ->
    gen_event:notify(?SERVER, {success, OpName}).

failure(OpName, Err) ->
    gen_event:notify(?SERVER, {failure, OpName, Err}).
