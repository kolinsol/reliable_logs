-module(json_processor).

-export([decode/1, validate/2, encode/1]).

decode(JSON) ->
    gen_server:call(jp_server, {decode, JSON}).

validate(JSON, SchemaName) ->
    gen_server:call(jp_server, {validate, JSON, SchemaName}).

encode(JSON) ->
    gen_server:call(jp_server, {encode, JSON}).
