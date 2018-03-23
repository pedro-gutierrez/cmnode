-module(cmweb_http).
-export([init/2,
         info/3]).

init(Req, State) ->
     {cowboy_loop, Req, State, hibernate}.

info(#{ type := Type, 
        status := Status, 
        body := Body }, Req, State) ->
    
    reply(Status, Type, Body, Req, State);

info(_, Req, State) ->
    reply(error, json, #{ error => unknown }, Req, State).

reply(Status, Type, Body, Req, State) ->
    {Code, Headers, Body} = reply(Status, Type, Body),
    cowboy_req:reply(Code, Headers, Body, Req),
    {stop, Req, State}.

reply(Status, Type, Body) ->
    {status(Status), headers(Type), body(Type, Body)}.

status(ok) -> 200;
status(invalid) -> 400;
status(not_found) -> 404;
status(forbidden) -> 401;
status(_) -> 500.

headers(json) -> 
    #{ <<"content-type">> => <<"application/json">> };

headers(text) ->
    #{ <<"content-type">> => <<"text/plain">> }.


body(json, Body) -> cmkit:jsone(Body);
body(_, Body) -> Body.
