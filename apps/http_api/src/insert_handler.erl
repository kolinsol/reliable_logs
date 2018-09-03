-module(insert_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    has_body(Req),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

has_body(Req) ->
    case cowboy_req:has_body(Req) of
        true -> is_json_body(Req);
        false -> {error, no_body}
    end.

is_json_body(Req) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        {<<"application/json">>, _Req2} ->
            {ok, Body, _Req3} = cowboy_req:body(Req),
            DecodedBody = json_processor:decode(Body),
            validate_body(DecodedBody);
        _ -> {error, wrong_content_type}
    end.

validate_body(Body) ->
    case json_processor:validate(Body, insert_schema) of
        {ok, _} -> io:format("success ~p~n", [Body]);
        {error, _Error} -> 
            io:format("failure ~p~n", [Body]),
            {error, wrong_json}
    end.
