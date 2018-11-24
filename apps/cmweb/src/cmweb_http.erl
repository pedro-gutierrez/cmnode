-module(cmweb_http).
-export([init/2,
         info/3]).

init(Req, #{app := App}=State) ->
    Start = cmkit:micros(),
    case cmconfig:app(App) of
        {ok, #{ debug := Debug }=Spec} -> 
            Pid = self(),
            Log = cmkit:log_fun(Debug),
            {ok, Data, Req2} = request_body(Req),
            BodyTime = cmkit:elapsed(Start),
            SessionTime = cmkit:elapsed(Start),
            {ok, Model, Config} = cmcore:init(Pid, Spec, Log),
            Spec2 = Spec#{ config => Config },
            InitTime = cmkit:elapsed(Start),
            Data2 = #{ method => cowboy_req:method(Req),
                       body => Data,
                       params => cowboy_req:bindings(Req),
                       headers => cowboy_req:headers(Req),
                       query => maps:from_list(cowboy_req:parse_qs(Req2)) },
            {ok, Model2} = cmcore:update(Pid, Spec2, Data2, Model, Log),
            UpdateTime = cmkit:elapsed(Start),
            {cowboy_loop, Req2, State#{ log => Log, 
                                        spec => Spec2,
                                        model => Model2,
                                        start => Start,
                                        body_time => BodyTime,
                                        session_time => SessionTime,
                                        init_time => InitTime,
                                        update_call_time => UpdateTime }};
        {error, E} -> 
            cmkit:danger({http, new, no_such_app, App, E}),
            reply_and_ok(error, json, #{}, Req, State#{ log => fun cmkit:log/1, 
                                                        start => Start })
    end.

info({update, Data}, Req, #{ spec := Spec,
                             model := Model,
                             log := Log } = State) ->
    {ok, Model2} = cmcore:update(self(), Spec, Data, Model, Log),
    {ok, Req, State#{ model => Model2 }};

info(terminate = Msg, Req, State) ->
    cmkit:warning({http, implement_me, Msg}),
    {ok, Req, State};

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


info(#{ status := Code, headers := Headers, body := Body }, Req, #{ app := App,
                                                                    start := Start,
                                                                    body_time := BodyTime,
                                                                    session_time := SessionTime,
                                                                    init_time := InitTime,
                                                                    update_call_time := UpdateTime,
                                                                    log := Log
                                                                  }=State) ->
    Elapsed = cmkit:elapsed(Start),
    Headers2 = binary_headers(Headers),
    Body2 = encoded_body(Headers2, Body),
    Req2 = cowboy_req:reply(Code, Headers2, Body2, Req),
    Elapsed2 = cmkit:elapsed(Start),
    Log({App, out, Code, Headers, Body, #{ total_time => Elapsed2,
                                                    body_time => BodyTime,
                                                    session_time => SessionTime - BodyTime,
                                                    init_time => InitTime - SessionTime,
                                                    update_call_time => UpdateTime - InitTime,
                                                    update_other_time => Elapsed - UpdateTime,
                                                    render_time => Elapsed2 - Elapsed
                                                  }}),
    {stop, Req2, State};
    
info(#{ status := Status } = Body, Req, State) when is_map(Body) ->
    reply_and_stop(Status, json, Body, Req, State);

info(Body, Req, State) when is_binary(Body) ->
    reply_and_stop(ok, binary, Body, Req, State);

info(Data, Req, State) ->
    reply_and_stop(error, json, #{ error => Data }, Req, State).

reply_and_stop(Status, Type, Body, Req, #{ log := Log,
                                           start := Start,
                                           app := App }=State) ->
    Elapsed = cmkit:elapsed(Start),
    {Code, Headers, EncodedBody} = reply(Status, Type, Body),
    Req2 = cowboy_req:reply(Code, Headers, EncodedBody, Req),
    Elapsed2 = cmkit:elapsed(Start),
    Log({App, out, Code, Headers, Body, #{ total_time => Elapsed2,
                                                  render_time => Elapsed2 - Elapsed}}),
    {stop, Req2, State}.

reply_and_ok(Status, Type, Body, Req, #{ log := Log,
                                         start := Start,
                                         app := App }=State) ->
    Elapsed = cmkit:elapsed(Start),
    {Code, Headers, EncodedBody} = reply(Status, Type, Body),
    Req2 = cowboy_req:reply(Code, Headers, EncodedBody, Req),
    Elapsed2 = cmkit:elapsed(Start),
    Log({App, out, Code, Headers, Body, #{ total_time => Elapsed2,
                                           render_time => Elapsed2 - Elapsed}}),
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
                {ok, Decoded} -> 
                    {ok, Decoded, Req2};
                {error, E} -> 
                    cmkit:warning({cmweb_http, body_error, Raw, E}),
                    {ok, Raw, Req2}
            end
    end.

request_body(Mime, Raw) ->
    case cmkit:is_json(Mime) of 
        true ->
            cmkit:jsond(Raw);
        false ->
            {ok, Raw}
    end.

binary_headers(Map) when is_map(Map) -> 
    maps:fold(fun(K, V, Out) ->
                      K2 = cmkit:to_bin(K),
                      V2 = cmkit:to_bin(V),
                      Out#{ K2 => V2 }
              end, #{}, Map).
