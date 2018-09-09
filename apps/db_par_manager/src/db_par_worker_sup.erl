-module(db_par_worker_sup).

-behaviour(supervisor).

-export([init/1, start_link/0, start_child/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([]) ->
    Worker = {db_par_worker, {db_par_worker, start_link, []},
              temporary, brutal_kill, worker, [db_par_worker]},
    Children = [Worker],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

