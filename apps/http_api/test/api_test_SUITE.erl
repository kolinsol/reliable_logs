-module(api_test_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([test/1]).

-include_lib("common_test/include/ct.hrl").

all() -> [test].

init_per_suite(Config) ->
    application:start(json_processor),
    Config.

end_per_suite(Config) ->
    application:stop(json_processor),
    Config.

test(_Config) ->
    1 = 1.
