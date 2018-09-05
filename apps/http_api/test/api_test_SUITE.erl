-module(api_test_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1,
        init_per_testcase/2, end_per_testcase/2]).
-export([ping_test/1]).

-include_lib("common_test/include/ct.hrl").

all() -> [ping_test].

init_per_suite(Config) ->
    application:start(json_processor),
    Config.

end_per_suite(Config) ->
    application:stop(json_processor),
    Config.

init_per_testcase(ping_test, Config) ->
    meck:new(http_api_event),
    meck:expect(http_api_event, request_received,
                fun(_A, _B) -> true end),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply,
                fun(Code, _Headers, Body, Req) -> {ok, {Code, Body, Req}} end),
    meck:expect(cowboy_req, method,
                fun({http_req, Method} = Req) -> {Method, Req} end),
    PingGetReq = {http_req,<<"GET">>},
    [{ping_get_req, PingGetReq} | Config].

end_per_testcase(ping_test, Config) ->
    meck:unload(http_api_event),
    meck:unload(cowboy_req),
    Config.

ping_test(Config) ->
    PingGetReq = ?config(ping_get_req, Config),
    {ok, {200, <<"{\"success\": true}">>, PingGetReq}, []} = ping_handler:handle(PingGetReq, []).
