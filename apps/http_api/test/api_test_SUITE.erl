-module(api_test_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1,
        init_per_testcase/2, end_per_testcase/2]).
-export([ping_test/1,
         insert_test/1]).

-include_lib("common_test/include/ct.hrl").

all() -> [ping_test, insert_test].

init_per_suite(Config) ->
    application:start(xmerl),
    application:start(jiffy),
    application:start(jesse),
    application:start(json_processor),
    Config.

end_per_suite(Config) ->
    application:stop(xmerl),
    application:stop(jiffy),
    application:stop(jesse),
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
                fun({http_req, {method, Method}} = Req) -> {Method, Req} end),

    PingGetReq = {http_req, {method, <<"GET">>}},
    [{ping_get_req, PingGetReq} | Config];
init_per_testcase(insert_test, Config) ->
    meck:new(http_api_event),
    meck:expect(http_api_event, request_received,
                fun(_A, _B) -> true end),

    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply,
                fun(Code, _Headers, Body, Req) ->
                        {ok, {Code, Body, Req}} end),
    meck:expect(cowboy_req, method,
                fun({http_req, {method, Method}, _Body, _CT} = Req) ->
                        {Method, Req} end),
    meck:expect(cowboy_req, has_body,
                fun({http_req, _Method, {body, _Body}, _CT}) -> true end),
    meck:expect(cowboy_req, header,
                fun(<<"content-type">>,
                    {http_req, _Method, _Body, {content_type, CT}} = Req) ->
                        {CT, Req} end),
    meck:expect(cowboy_req, body,
                fun({http_req, _Method, {body, Body}, _CT} = Req) ->
                        {ok, Body, Req} end),

    meck:new(db_manager),
    meck:expect(db_manager, insert, fun(_A) -> true end),

    HttpApiPrivDir = code:priv_dir(http_api),
    TestDataDir = filename:join(HttpApiPrivDir, "test_data"),

    InsertBodyPath = filename:join([TestDataDir, "insert_body.json"]),
    {ok, InsertBody} = file:read_file(InsertBodyPath),

    InsertPostReq = {http_req,
                     {method, <<"POST">>},
                     {body, InsertBody},
                     {content_type, <<"application/json">>}},
    [{insert_post_req, InsertPostReq} | Config].

end_per_testcase(ping_test, Config) ->
    meck:unload(http_api_event),
    meck:unload(cowboy_req),
    Config;
end_per_testcase(insert_test, Config) ->
    meck:unload(http_api_event),
    meck:unload(cowboy_req),
    meck:unload(db_manager),
    Config.

ping_test(Config) ->
    PingGetReq = ?config(ping_get_req, Config),
    {ok, {200, <<"{\"success\": true}">>, PingGetReq}, []} =
        ping_handler:handle(PingGetReq, []).

insert_test(Config) ->
    InsertPostReq = ?config(insert_post_req, Config),
    {ok, {200, <<"{\"success\": true}">>, InsertPostReq}, []} =
        insert_handler:handle(InsertPostReq, []).
