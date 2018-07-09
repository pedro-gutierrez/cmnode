-module(cmhttp).
-export([stream/1, get/1, get/2, post/3]).

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

get(Url) ->
    get(Url, #{}).

get(Url, Headers) ->
    Url2 = encoded_url(Url),
    Headers2 = encoded_headers(Headers),
    handle(httpc:request(get, {Url2, Headers2},[],[])).


post(Url, #{ 'content-type' := CT }=Headers, Data) ->
    Url2 = encoded_url(Url),
    Headers2 = encoded_headers(Headers),
    Mime = cmkit:to_list(CT),
    Encoded = encoded_body(Mime, Data),
    handle(httpc:request(post,{ Url2, Headers2, Mime, Encoded},[],[]));

post(Url, Headers, Data) ->
    post(Url, Headers#{ 'content-type' => <<"application/json">> }, Data).

handle({error,socket_closed_remotely}) ->
    {error, closed};

handle({error,{failed_connect, _}}) ->
    {error, failed_connect};

handle({error,E}) ->
    {error, E};

handle({ok, {{_, Code, _}, Headers, Body}}) ->
    DecodedBody = case decoded_mime(Headers) of
        {error, no_content_type} -> Body;
        {ok, json} ->
            case cmkit:jsond(Body) of 
                {ok, Term} -> Term;
                _ -> Body
            end;
        {ok, _} -> Body
    end,

    {ok, #{ status => Code,
            headers => decoded_headers(Headers, #{}), 
            body => DecodedBody }}.

decoded_mime(Headers) ->
    case lists:keyfind("content-type", 1, Headers) of 
        false -> {error, no_content_type};
        {"content-type", CT} ->
            case string:str(CT, "json") of 
                0 -> {ok, CT};
                _ -> {ok, json}
            end
    end.

encoded_body(Mime, Data) when is_list(Mime) ->
    encoded_body(cmkit:to_bin(Mime), Data);

encoded_body(<<"application/json">>, Data) ->
    cmkit:jsone(Data);

encoded_body(_, Data) ->
    Data.

encoded_url(Url) -> cmkit:to_list(Url).
encoded_headers(H) when is_map(H) ->
    [{ cmkit:to_list(K), cmkit:to_list(V)} || {K, V} <- maps:to_list(H)].

decoded_headers([], Out) -> Out;
decoded_headers([{K, V}|Rem], Out) ->
    BinKey = cmkit:to_bin(K),
    BinValue = cmkit:to_bin(V),
    decoded_headers(Rem, Out#{ BinKey => BinValue }).

