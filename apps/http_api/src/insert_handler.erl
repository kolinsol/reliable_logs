-module(insert_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    {Method, _TempReq} = cowboy_req:method(Req),
    http_api_event:request_received(?MODULE, Method),

    case common_handler:process_request(Req, fun validate_body/1) of
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

validate_body(Body) ->
    case json_processor:validate(Body, insert_schema) of
        {ok, _} ->
            db_manager:insert(Body),
            {ok, <<"request sent">>};
        {error, _Error} -> 
            {error, <<"invalid json">>, 400}
    end.
