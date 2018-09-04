%%%-------------------------------------------------------------------
%% @doc http_api top level supervisor.
%%%-------------------------------------------------------------------

-module(http_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    EventManager = {http_api_event, {http_api_event, start_link, []},
                    permanent, 2000, worker, [http_api_event]},
    Children = [EventManager],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
