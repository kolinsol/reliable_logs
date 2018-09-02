%%%-------------------------------------------------------------------
%% @doc db_manager top level supervisor.
%%%-------------------------------------------------------------------

-module(db_manager_sup).

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
    DBServer = {db_server, {db_server, start_link, []},
                permanent, 2000, worker, [db_server]},
    Children = [DBServer],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
