%%%-------------------------------------------------------------------
%% @doc json_processor top level supervisor.
%%%-------------------------------------------------------------------

-module(json_processor_sup).

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
     JSONProcessingServer = {jp_server, {jp_server, start_link, []},
                             permanent, 2000, worker, [jp_server]},
     Children = [JSONProcessingServer],
     RestartStrategy = {one_for_one, 4, 3600},
     {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
