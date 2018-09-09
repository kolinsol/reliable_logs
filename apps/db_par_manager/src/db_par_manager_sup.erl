%%%-------------------------------------------------------------------
%% @doc db_par_manager top level supervisor.
%%%-------------------------------------------------------------------

-module(db_par_manager_sup).

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
     WorkerSup = {db_par_worker_sup, {db_par_worker_sup, start_link, []},
                  permanent, 2000, supervisor, [db_par_worker_sup]},
     ParServer = {db_par_manager, {db_par_manager, start_link, []},
                  permanent, 2000, worker, [db_par_manager]},
     Children = [WorkerSup, ParServer],
     RestartStrategy = {one_for_one, 4, 3600},
     {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
