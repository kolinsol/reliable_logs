-module(insert_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    case process_request(Req) of
        {ok, _} ->
            {ok, Req2} = cowboy_req:reply(200, [
                {<<"content-type">>, <<"application/json">>}
            ], <<"{\"success\": true}">>, Req),
            {ok, Req2, State};
        {error, ErrorMessgae, StatusCode} ->
            ResultJSON = {[{<<"success">>, false},
                           {<<"reason">>, ErrorMessgae}]},
            EncodedResultJSON = json_processor:encode(ResultJSON),
            {ok, Req2} = cowboy_req:reply(StatusCode, [
                {<<"content-type">>, <<"application/json">>}
            ], EncodedResultJSON, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

process_request(Req) ->
    has_body(Req).

has_body(Req) ->
    case cowboy_req:has_body(Req) of
        true -> is_json_body(Req);
        false -> {error, <<"no body">>, 400}
    end.

is_json_body(Req) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        {<<"application/json">>, _Req2} ->
            {ok, Body, _Req3} = cowboy_req:body(Req),
            DecodedBody = json_processor:decode(Body),
            validate_body(DecodedBody);
        _ -> {error, <<"wrong content type">>, 400}
    end.

validate_body(Body) ->
    case json_processor:validate(Body, insert_schema) of
        {ok, _} ->
            db_manager:insert(Body),
            {ok, <<"request sent">>};
        {error, _Error} -> 
            {error, <<"invalid json">>, 400}
    end.
