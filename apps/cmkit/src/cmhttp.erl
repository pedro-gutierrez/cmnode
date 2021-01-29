-module(cmhttp).
-export([stream/1,
         method/1,
         do/1,
         get/1, 
         get/2,
         delete/1,
         delete/2,
         post/1,
         post/2,
         post/3,
         post/4,
         put/1,
         put/2,
         put/3, 
         encodedQs/1]).
-define(EMPTY_BODY, <<>>).
-define(EMPTY_OPTS, #{}).
-define(DEFAULT_MIME, <<"application/octect-stream">>).


stream(#{ method := Method,
          url := Url,
          headers := Headers,
          context := #{ data := Ctx,
                        callback := {M, F}}}) ->

    cmkit:log({cmhttp, stream, Url, Headers, M, F, Ctx}),
    Url2 = encoded_url(Url),
    Headers2 = encoded_headers(Headers),

    Map = fun ({_, stream_start, H}) ->
                  maps:merge(Ctx, #{ event => start,
                                     data => #{ status => 200,
                                                headers => decoded_headers(H, #{})}});
              ({_, stream, BinBodyPart}) -> 
                  maps:merge(Ctx, #{ event => data,
                                     data => BinBodyPart });
              ({_, stream_end, H}) -> 
                  maps:merge(Ctx, #{ event => 'end',
                                     data => decoded_headers(H, #{})});
              ({_, {{_, Status, _}, H, Body}}) -> 
                  maps:merge(Ctx, #{ event => 'error',
                                     data => #{ status => Status,
                                                headers => decoded_headers(H, #{}),
                                                body => Body }});
              (_) ->
                  ignore
          end,

    Recv = fun(Ev) ->
                   case Map(Ev) of 
                       ignore -> 
                           cmkit:log({cmhttp, stream, ignored, Ev});
                       Data ->
                           spawn(M, F, [Data])
                   end
           end,

    httpc:request(Method, {Url2, Headers2}, [], [{sync, false},
                                                 {stream, self}, 
                                                 {receiver, Recv}]);

stream(#{ method := _,
          url := _,
          context := _} = Q) -> 

    stream(Q#{ headers => #{} }).

do(Req0) ->
    Req = with_method(Req0),
    {Elapsed, Out} = timer:tc(fun() -> req(Req) end),
    Millis = trunc(Elapsed/1000),
    Log = cmkit:log_fun(Req),
    case Out of 
        {ok, Res} ->
            Log({http, #{ in => Res, 
                          out => Req0,
                          millis => Millis }}),
            {ok, Res#{ req => Req0,
                       millis => Millis }};
        {error, E} ->
            Log({http, #{ out => Req0, 
                          in => E,
                          millis => Millis }}),
            {error, E}
    end.

method(get) -> get;
method(<<"get">>) -> get;
method(<<"GET">>) -> get;

method(post) -> post;
method(<<"post">>) -> post;
method(<<"POST">>) -> post;

method(delete)-> delete;
method(<<"delete">>) -> delete;
method(<<"DELETE">>) -> delete;

method(put)-> put;
method(<<"put">>) -> put;
method(<<"PUT">>) -> put.


with_method(#{ method := V }=S) -> 
    S#{ method => method(V) };

with_method(S) ->
    S#{ method => get }.


req(#{ method := M,
       url := Url,
       headers := Headers,
       body := Body } = Spec) ->

    cmhttp:M(withQs(Url, Spec), Headers, Body);

req(#{ method := M,
       url := Url,
       headers := Headers } = Spec) ->

    cmhttp:M(withQs(Url, Spec), Headers);

req(#{ method := M,
       url := Url,
       body:= Body } = Spec) ->

    cmhttp:M(withQs(Url, Spec), Url, #{ <<"content-type">> => ?DEFAULT_MIME }, Body);

req(#{ method := M,
       url := Url }  =Spec) ->

    cmhttp:M(withQs(Url, Spec)).


get(Url) ->
    get(Url, #{}).

get(Url, Headers) ->
    Url2 = encoded_url(Url),
    Headers2 = encoded_headers(Headers),
    handle(httpc:request(get, {Url2, Headers2},[],[]), ?EMPTY_OPTS).

delete(Url) ->
    delete(Url, #{}).

delete(Url, Headers) ->
    Url2 = encoded_url(Url),
    Headers2 = encoded_headers(Headers),
    handle(httpc:request(delete, {Url2, Headers2},[],[]), ?EMPTY_OPTS).

put(Url) ->
    cmhttp:put(Url, #{}).

put(Url, Headers) ->
    cmhttp:put(Url, Headers, ?EMPTY_BODY).

put(Url, #{ 'content-type' := _}=Headers, Data) ->
    send_body(put, Url, Headers, Data, ?EMPTY_OPTS);

put(Url, Headers, Data) ->
    cmhttp:put(Url, Headers#{ 'content-type' => <<"application/json">> }, Data).

post(Url) ->
    post(Url, #{}).

post(Url, Headers) ->
    post(Url, Headers, ?EMPTY_BODY).

post(Url, #{ 'content-type' := _}=Headers, Data) ->
    post(Url, Headers, Data, ?EMPTY_OPTS);

post(Url, Headers, Data) ->
    post(Url, Headers#{ 'content-type' => <<"application/json">> }, Data, ?EMPTY_OPTS).

post(Url, Headers, Data, Opts) ->
    send_body(post, Url, Headers, Data, Opts).


send_body(Method, Url, #{ 'content-type' := CT }=Headers, Data, Opts) ->
    Url2 = encoded_url(Url),
    Headers2 = encoded_headers(Headers),
    Mime = cmkit:to_list(CT),
    Encoded = encoded_body(Mime, Data),
    debug(Method, Url2, Headers2, Mime, Encoded, Opts),
    handle(httpc:request(Method, { Url2, Headers2, Mime, Encoded},[],[]), Opts).

debug(Method, Url, Headers, Mime, Encoded, #{ debug := true }) ->
    cmkit:log({http, #{ method => Method, 
                        url => Url, 
                        mime => Mime,
                        headers => Headers,
                        body_size => size(Encoded)}});

debug(_, _, _, _, _, _) ->
    ok.


handle({error,socket_closed_remotely}, _) ->
    {error, closed};

handle({error,{failed_connect, _}}, _) ->
    {error, failed_connect};

handle({error,E}, _) ->
    {error, E};

handle({ok, {{_, Code, _}, Headers, Body}}, Opts) ->
    DecodedMime = decoded_mime(Headers),
    DecodedBody = case DecodedMime of
                      missing -> Body;
                      json ->
                          case cmkit:jsond(Body) of 
                              {ok, Term} -> Term;
                              _ -> Body
                          end;
                      xml ->
                          case cmkit:xmld(Body) of 
                              {ok, Term} -> Term;
                              _ -> 
                                  Body
                          end;
                      _ -> Body
                  end,

    {ok, with_raw_body(Body, #{ status => Code,
                                headers => decoded_headers(Headers, #{}), 
                                mime => DecodedMime,
                                body => DecodedBody }, Opts)}.


with_raw_body(Body,  Res, #{ raw := true }) ->
    Res#{ raw => Body };

with_raw_body(_, Res, _) -> Res.


decoded_mime(Headers) ->
    case lists:keyfind("content-type", 1, Headers) of 
        false -> missing;
        {"content-type", CT} ->
            case string:str(CT, "json") of 
                0 -> 
                    case string:str(CT, "xml") of 
                        0 -> 
                            CT;
                        _ -> 
                            xml
                    end;
                _ -> json
            end
    end.

encoded_body(Mime, Data) when is_list(Mime) ->
    encoded_body(cmkit:to_bin(Mime), Data);

encoded_body(Mime, Data) when is_binary(Mime) ->
    case cmkit:is_json(Mime) of 
        true ->
            cmkit:jsone(Data);
        false ->
            encode_binary(Data)
    end.

encode_binary(Bin) when is_binary(Bin) -> Bin;
encode_binary(List) when is_list(List) -> 
    cmkit:to_bin(List);

%%case cmkit:is_string(List) of 
%%    true -> 
%%        cmkit:warning({http, encode_binary, is_string}),
%%        cmkit:to_bin(List);
%%    false ->
%%        cmkit:warning({http, encode_binary, is_not_string}),
%%        erlang:term_to_binary(List)
%%end;
encode_binary(Other) ->
    erlang:term_to_binary(Other).

encoded_url(Url) -> cmkit:to_list(Url).
encoded_headers(H) when is_map(H) ->
    [{ cmkit:to_list(K), cmkit:to_list(V)} || {K, V} <- maps:to_list(H)].

decoded_headers([], Out) -> Out;
decoded_headers([{K, V}|Rem], Out) ->
    BinKey = cmkit:to_bin(K),
    BinValue = cmkit:to_bin(V),
    decoded_headers(Rem, Out#{ BinKey => BinValue }).

withQs(Url, #{ query := Qs}) when map_size(Qs) > 0 ->
    EncodedQs = encodedQs(Qs),
    <<Url/binary, EncodedQs/binary>>;

withQs(Url, _) -> Url.



encodedQs(Map) when is_map(Map) ->
    Params = cmkit:bin_join(maps:fold(fun(K, V, Acc) ->
                                              add_to_qs(K, V, Acc)
                                      end, [], Map), <<"&">>),
    <<"?", Params/binary>>;

encodedQs(Bin) when is_binary(Bin) ->
    Bin.


add_to_qs(_, [], Qs) -> Qs;
add_to_qs(K, [V|Rest], Qs) ->
    add_to_qs(K, Rest, add_to_qs(K, V, Qs));

add_to_qs(K, V, Qs) ->
    KBin = cmkit:to_bin(K),
    VBin = cmkit:to_bin(V),
    [<<KBin/binary, "=", VBin/binary>>|Qs].

