-module(common_handler).

-export([process_request/2]).

process_request(Req, Validator) ->
    has_body(Req, Validator).

has_body(Req, Validator) ->
    case cowboy_req:has_body(Req) of
        true ->
            is_json_body(Req, Validator);
        false -> {error, <<"no body">>, 400}
    end.

is_json_body(Req, Validator) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        {<<"application/json">>, _Req2} ->
            {ok, Body, _Req3} = cowboy_req:body(Req),
            decode_body(Body, Validator);
        _ -> {error, <<"wrong content type">>, 400}
    end.

decode_body(Body, Validator) ->
    case json_processor:decode(Body) of
        {error, _Error} ->
            {error, <<"malformed json">>, 400};
        DecodedBody ->
            Validator(DecodedBody)
    end.
