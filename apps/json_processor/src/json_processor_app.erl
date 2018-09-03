%%%-------------------------------------------------------------------
%% @doc json_processor public API
%%%-------------------------------------------------------------------

-module(json_processor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    PrivDir = code:priv_dir(json_processor),
    SchemasDir = filename:join(PrivDir, "schemas"),

    InsertSchemaPath = filename:join([SchemasDir, "insert_schema.json"]),
    {ok, InsertSchema} = file:read_file(InsertSchemaPath),
    DecodedInsertSchema = jiffy:decode(InsertSchema),
    jesse:add_schema(insert_schema, DecodedInsertSchema),

    SelectSchemaPath = filename:join([SchemasDir, "select_schema.json"]),
    {ok, SelectSchema} = file:read_file(SelectSchemaPath),
    DecodedSelectSchema = jiffy:decode(SelectSchema),
    jesse:add_schema(select_schema, DecodedSelectSchema),

    json_processor_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
