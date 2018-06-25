-module(cmhttp).
-export([ get/1, get/2, post/3 ]).

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
    handle(httpc:request(post,{ Url2, Headers2, Mime, Encoded},[],[])).

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

