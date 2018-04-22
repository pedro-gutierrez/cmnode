-module(cmweb_http).
-export([init/2,
         info/3]).

init(Req, #{app := App}=State) ->
    case cmconfig:app(App) of
        {ok, #{ debug := Debug }=Spec} -> 
            Log = cmkit:log_fun(Debug),
            case request_body(Req) of 
                {error, _} -> 
                    reply(invalid, json, #{ error => json }, Req, State);
                {ok, Data, Req2} ->
                    {ok, Session } = cmsession:new(App),
                    ok = cmcore:init(Data, Spec, Session),
                    {cowboy_loop, Req2, State#{ log => Log }, hibernate}
            end;
        {error, E} -> 
            cmkit:log({http, new, invalid_app, App, E}),
            {stop, E}
    end.

info(#{ status := Status } = Body, Req, State) when is_map(Body) ->
    reply(Status, json, Body, Req, State);

info(Body, Req, State) when is_binary(Body) ->
    reply(ok, binary, Body, Req, State);

info(Data, Req, State) ->
    reply(error, json, #{ error => Data }, Req, State).

reply(Status, Type, Body, Req, State) ->
    {Code, Headers, EncodedBody} = reply(Status, Type, Body),
    cowboy_req:reply(Code, Headers, EncodedBody, Req),
    {stop, Req, State}.

reply(Status, Type, Body) ->
    {status(Status), headers(Type), response_body(Type, Body)}.

status(ok) -> 200;
status(invalid) -> 400;
status(not_found) -> 404;
status(forbidden) -> 401;
status(_) -> 500.

headers(json) -> 
    H = headers(),
    H#{ <<"content-type">> => <<"application/json">> };

headers(_) ->
    H = headers(),
    H#{ <<"content-type">> => <<"text/plain">> }.

headers() ->
    #{ <<"server">> => <<"cmnode">>, 
       <<"hostname">> => cmkit:host() }.


response_body(json, Body) -> cmkit:jsone(Body);
response_body(_, Body) -> Body.

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
    cmkit:jsond(Raw).
