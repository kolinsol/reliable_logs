-module(db_manager).

-export([insert/1, select/1]).

insert(JSON) ->
    gen_server:cast(db_server, {insert, JSON}).

select(Query) ->
    gen_server:call(db_server, {select, Query}).
