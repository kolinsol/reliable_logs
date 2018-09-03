-module(db_manager).

-export([insert/1]).

insert(JSON) ->
    gen_server:cast(db_server, {insert, JSON}).
