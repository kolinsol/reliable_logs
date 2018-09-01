-module(json_processor).

-export([decode/1]).

decode(RawJSON) ->
    gen_server:call(jp_server, {decode, RawJSON}).
