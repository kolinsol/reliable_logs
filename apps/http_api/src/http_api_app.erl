%%%-------------------------------------------------------------------
%% @doc http_api public API
%%%-------------------------------------------------------------------

-module(http_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_',
         [
          {"/ping", ping_handler, []},
          {"/insert", insert_handler, []},
          {"/select", select_handler, []}
         ]}
    ]),
    {ok, _} = cowboy:start_http(http_listener, 100,
                                [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    case http_api_sup:start_link() of
        {ok, Pid} ->
            http_api_event_logger:add_handler(),
            {ok, Pid};
        Other -> {error, Other}
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
