-module(cmweb_http).
-export([init/2,
         info/3]).
-export([default_headers/0]).

init(#{ method := Method }=Req, #{ instruments := #{ increment := IncrFun },
                                   app := App, effects := Effects}=State) ->
    IncrFun(),
    Start = cmkit:micros(),
    case cmconfig:app(App) of
        {ok, #{ debug := Debug } = Spec} -> 
            Pid = self(),
            Log = cmkit:log_fun(Debug),
            {ok, Data, Req2} = request_body(Req, Log),
            BodyTime = cmkit:elapsed(Start),
            SessionTime = cmkit:elapsed(Start),
            case cmcore:init(Pid, Spec, Log, Effects) of 
                {ok, Model, Config} -> 
                    Spec2 = Spec#{ config => Config },
                    InitTime = cmkit:elapsed(Start),
                    Data2 = #{ method => cowboy_req:method(Req),
                               body => Data,
                               params => cowboy_req:bindings(Req),
                               headers => cowboy_req:headers(Req),
                               query => maps:from_list(cowboy_req:parse_qs(Req2)) },
                    case cmcore:update(Pid, Spec2, #{ effect => web,
                                                      data => Data2 }, Model, Log, Effects) of 
                        {ok, Model2, Spec3} ->
                            UpdateTime = cmkit:elapsed(Start),
                            {cowboy_loop, Req2, State#{ log => Log, 
                                                        method => Method,
                                                        spec => Spec3,
                                                        model => Model2,
                                                        start => Start,
                                                        body_time => BodyTime,
                                                        session_time => SessionTime,
                                                        init_time => InitTime,
                                                        update_call_time => UpdateTime }, hibernate};
                        {error, E} ->
                            cmkit:danger({http, App, update, E}),
                            reply_and_stop(error, json, #{}, Req, State#{ log => fun cmkit:log/1,
                                                                          method => Method,
                                                                          start => Start })
                    end;
                {error, E} ->
                    cmkit:danger({http, App, init, E}),
                    reply_and_stop(error, json, #{}, Req, State#{ log => fun cmkit:log/1,
                                                                  methos => Method,
                                                                  start => Start })
            end;
        {error, E} -> 
            cmkit:danger({http, new, no_such_app, App, E}),
            reply_and_stop(error, json, #{}, Req, State#{ log => fun cmkit:log/1, 
                                                          method => Method,
                                                          start => Start })
    end.

info({update, Data}, Req, #{ app := App,
                             spec := Spec,
                             model := Model,
                             log := Log,
                             effects := Effects } = State) ->
    case cmcore:update(self(), Spec, Data, Model, Log, Effects) of 
        {ok, Model2, Spec2} ->
            {ok, Req, State#{ model => Model2, 
                              spec => Spec2 }};
        {error, E} ->
            cmkit:danger({http, App, update, E}),
            reply_and_stop(error, json, #{}, Req, State)
    end;

info({terminate, Data}, Req, State) ->
    info(Data, Req, State);

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
                                                                    method := Method,
                                                                    start := Start,
                                                                    body_time := BodyTime,
                                                                    session_time := SessionTime,
                                                                    init_time := InitTime,
                                                                    update_call_time := UpdateTime,
                                                                    log := Log,
                                                                    instruments := #{ duration := DurationFun,
                                                                                      decrement := DecrFun }
                                                                  }=State) ->
    Headers2 = binary_headers(Headers),
    Body2 = encoded_body(Headers2, Body),
    Elapsed = cmkit:elapsed(Start),
    Headers3 = Headers2#{ <<"elapsed">> => cmkit:to_bin(Elapsed) },
    Req2 = cowboy_req:reply(Code, Headers3, Body2, Req),
    Elapsed2 = cmkit:elapsed(Start),
    Log({App, out, #{ status => Code, 
                      headers => Headers,
                      body => Body, 
                      times => #{ total => Elapsed2,
                                  body  => BodyTime,
                                  init  => InitTime - SessionTime,
                                  update_call => UpdateTime - InitTime,
                                  update_other  => Elapsed - UpdateTime,
                                  render => Elapsed2 - Elapsed }}}),

    DurationFun(Method, Code, Elapsed2/1000),
    DecrFun(),

    {stop, Req2, State};

info(Data, Req, State) ->
    reply_and_stop(error, json, #{ error => Data }, Req, State).

reply_and_stop(Status, Type, Body, Req, #{ method := Method, 
                                           log := Log,
                                           start := Start,
                                           instruments := #{ duration := DurationFun,
                                                             decrement := DecrFun },
                                           app := App }=State) ->
    {Code, Headers, EncodedBody} = reply(Status, Type, Body),
    Elapsed = cmkit:elapsed(Start),
    Headers2 = Headers#{ <<"elapsed">> => cmkit:to_bin(Elapsed) },
    Req2 = cowboy_req:reply(Code, Headers2, EncodedBody, Req),
    Elapsed2 = cmkit:elapsed(Start),
    Log({App, out, Code, Headers, Body, #{ total_time => Elapsed2,
                                           render_time => Elapsed2 - Elapsed}}),

    DurationFun(Method, Code, Elapsed2/1000),
    DecrFun(),
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
    H = default_headers(),
    H#{ <<"content-type">> => <<"application/json">> };

headers(_) ->
    H = default_headers(),
    H#{ <<"content-type">> => <<"text/plain">> }.

default_headers() ->
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

request_body(Req, Log) ->
    case cowboy_req:has_body(Req) of 
        false -> 
            {ok, #{}, Req};
        true ->
            case cowboy_req:parse_header(<<"content-type">>, Req) of 
                {CT1, CT2, _} -> 
                    request_body(CT1, CT2, Req, Log);
                _ ->
                    request_body(unknown, unknown, Req, Log)
            end
    end.


request_body(<<"application">>, <<"json">>, Req, Log) ->
    {ok, Raw, Req2} = cowboy_req:read_body(Req),
    case cmkit:jsond(Raw) of 
        {ok, Decoded} -> 
            {ok, Decoded, Req2};
        {error, E} ->
            Log({cmweb_http, body_error, cmkit:printable(Raw), E, Req}),
            {ok, Raw, Req2}
    end;

request_body(<<"multipart">>, <<"form-data">>, Req, _) ->
    multipart_body(Req, #{});

request_body(_, _, Req, _) ->
    {ok, Raw, Req2} = cowboy_req:read_body(Req),
    {ok, Raw, Req2}.


multipart_body(Req0, Body) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {ok, Body0, Req2} = cowboy_req:read_part_body(Req1),
                    multipart_body(Req2, Body#{ FieldName => Body0 });
                {file, FieldName, Filename, CType} ->
%%%
%%% TODO: streaming
                    case multipart_file(Req1) of 
                        {ok, FileData, Req2} ->
                            multipart_body(Req2, Body#{ FieldName => #{ name => Filename,
                                                                        type => CType,
                                                                        content => FileData }});
                        {error, E, Req2} ->
                            multipart_body(Req2, Body#{ FieldName => #{ name => Filename,
                                                                        type => CType,
                                                                        error => E }})
                    end
            end;
        {done, Req} ->
            {ok, Body, Req}
    end.

multipart_file(Req) ->
    multipart_file(Req, <<>>).

multipart_file(Req0, Out) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, Chunk, Req} ->
            {ok, <<Out/binary, Chunk/binary>>, Req};
        {more, Chunk, Req} ->
            multipart_file(Req, <<Out/binary, Chunk/binary>>)
    end.


binary_headers(Map) when is_map(Map) -> 
    maps:fold(fun(K, V, Out) ->
                      K2 = cmkit:to_bin(K),
                      V2 = cmkit:to_bin(V),
                      Out#{ K2 => V2 }
              end, #{}, Map).
