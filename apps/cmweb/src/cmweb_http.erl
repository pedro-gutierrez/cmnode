-module(cmweb_http).
-export([init/2,
         info/3]).

init(Req, #{app := App}=State) ->
    case cmconfig:app(App) of
        {ok, #{ debug := Debug }=Spec} -> 
            Log = cmkit:log_fun(Debug),
            case request_body(Req) of 
                {error, E} -> 
                    cmkit:log({cmweb, body_error, E}),
                    reply_and_ok(invalid, json, #{ error => json }, Req, State);
                {ok, Data, Req2} ->
                    case cmsession:new(App) of 
                        {ok, #{ id := Id}=Session} ->
                            ok = cmcore:init(Spec, Session),
                            Data2 = #{ method => cowboy_req:method(Req),
                                       body => Data, 
                                       headers => cowboy_req:headers(Req),
                                       query => maps:from_list(cowboy_req:parse_qs(Req2)) },
                            cmcore:update(Id, Data2),
                            {cowboy_loop, Req2, State#{ log => Log }};
                        {error, E} ->
                            cmkit:danger({http, error, App, session, E}),
                            reply_and_ok(error, json, #{}, Req, State)
                    end
            end;
        {error, E} -> 
            cmkit:danger({http, new, no_such_app, App, E}),
            reply_and_ok(error, json, #{}, Req, State)
    end.

info({stream, start, Headers}, Req, State) ->
    Headers2 = binary_headers(Headers),
    Req2 = cowboy_req:stream_reply(200, Headers2, Req),
    {ok, Req2, State};

info({stream, data, Data}, Req, State) -> 
    cowboy_req:stream_body(Data, nofin, Req),
    {ok, Req, State};

info({stream, 'end', Headers}, Req, State) -> 
    Headers2 = binary_headers(Headers),
    cowboy_req:stream_trailers(Headers2, Req),
    {stop, Req, State};


info(#{ status := Code, headers := Headers, body := Body }, Req, State) ->
    Headers2 = binary_headers(Headers),
    Body2 = encoded_body(Headers2, Body),
    cmkit:log({http, out, Code, Headers, Body2}),
    Req2 = cowboy_req:reply(Code, Headers2, Body2, Req),
    {stop, Req2, State};
    
info(#{ status := Status } = Body, Req, State) when is_map(Body) ->
    reply_and_stop(Status, json, Body, Req, State);

info(Body, Req, State) when is_binary(Body) ->
    reply_and_stop(ok, binary, Body, Req, State);

info(Data, Req, State) ->
    reply_and_stop(error, json, #{ error => Data }, Req, State).

reply_and_stop(Status, Type, Body, Req, State) ->
    {Code, Headers, EncodedBody} = reply(Status, Type, Body),
    cmkit:log({http, out, Code, Headers, EncodedBody}),
    Req2 = cowboy_req:reply(Code, Headers, EncodedBody, Req),
    {stop, Req2, State}.

reply_and_ok(Status, Type, Body, Req, State) ->
    {Code, Headers, EncodedBody} = reply(Status, Type, Body),
    cmkit:log({http, Code, Headers, EncodedBody}),
    Req2 = cowboy_req:reply(Code, Headers, EncodedBody, Req),
    {ok, Req2, State}.

reply(Status, Type, Body) ->
    {status(Status), headers(Type), response_body(Type, Body)}.

status(ok) -> 200;
status(200) -> 200;
status(invalid) -> 400;
status(not_found) -> 404;
status(404) -> 404;
status(forbidden) -> 401;
status(401) -> 401;
status(_) -> 500.

headers(json) -> 
    H = headers(),
    H#{ <<"content-type">> => <<"application/json">> };

headers(_) ->
    H = headers(),
    H#{ <<"content-type">> => <<"text/plain">> }.

headers() ->
    #{ <<"server">> => <<"cmnode">>, 
       <<"connection">> => <<"close">>,
       <<"hostname">> => cmkit:host() }.

encoded_body(Headers, Body) -> 
    response_body(mime(Headers), Body).

response_body(json, Body) -> cmkit:jsone(Body);
response_body(_, Body) -> Body.

mime(#{ <<"content-type">> := <<"application/json", _/binary>> }) -> json;
mime(#{ <<"content-type">> := <<"application/json">> }) -> json;
mime(#{ <<"content-type">> := <<"application/javascript">> }) -> json;
mime(_) -> other.

request_body(Req) ->
    case cowboy_req:has_body(Req) of 
        false -> 
            {ok, #{}, Req};
        true ->
            CT = cowboy_req:header(<<"content-type">>, Req, <<"application/json">>),
            {ok, Raw, Req2} = cowboy_req:read_body(Req),
            case request_body(CT, Raw) of 
                {ok, Decoded} -> {ok, Decoded, Req2};
                {error, E} -> {error, E}
            end
    end.

request_body(<<"application/json">>, Raw) ->
    cmkit:jsond(Raw);

request_body(_, Raw) -> {ok, Raw}.

binary_headers(Map) when is_map(Map) -> 
    maps:fold(fun(K, V, Out) ->
                      K2 = cmkit:to_bin(K),
                      V2 = cmkit:to_bin(V),
                      Out#{ K2 => V2 }
              end, #{}, Map).
