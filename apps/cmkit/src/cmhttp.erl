-module(cmhttp).
-export([ get/1,
          get/2,
          get/4,
          post/3,
          post/4,
          post/5,
          post/6,
          url/4
        ]).

get(Url) ->
    get(Url, #{}).

get(Url, Params) ->
    get(Url, Params, none).

get(Url, Params, User, Passwd) ->
    get(Url, Params, auth(User, Passwd)).
        
get(Url, Params, Auth) ->
    Headers = headers_with_auth(Auth, []),
    handle(httpc:request(get,{ url(Url, Params), Headers },[],[])).

post(Url, Mime, Body) ->
    post(Url, #{}, Mime, Body).

post(Url, Params, Mime, Body) ->
    post(Url, Params, Mime, Body, none).

post(Url, Params, Mime, Body, User, Passwd) ->
    post(Url, Params, Mime, Body, auth(User, Passwd)).

post(Url, Params, Mime, Body, Auth) ->
    Headers = headers_with_auth(Auth, []),
    Data = encoded_body(Mime, Body),
    handle(httpc:request(post,{ url(Url, Params), Headers, cmkit:to_list(Mime), Data},[],[])).

handle({ok, {{_, 200, _}, _, []}}) -> 
    ok;

handle({ok, {{_, 200, _}, Headers, Body}}) when is_list(Body) ->
    case lists:keyfind("content-type", 1, Headers) of 
        {"content-type", "application/json; charset=UTF-8"} ->
            {ok, cmkit:jsond(Body)};
        {"content-type", "application/json"} ->
            {ok, cmkit:jsond(Body)};
        {"content-type", CT} ->
            {ok, CT, Body};
        false ->
            {error, {no_content_type, Body}}
    end;

handle({ok, {{_, 404, _}, _, _}}) ->
    {error, not_found};

handle({ok, {{_, 409, _}, _, _}}) ->
    {error, conflict};

handle({ok, {{_, 401, _}, _, _}}) ->
    {error, unauth};

handle({ok, {{_, _, _}, _, Body}}) ->
    {error, Body}.

encoded_body("application/json", Data) ->
    cmkit:jsone(Data);

encoded_body(<<"application/json">>, Data) ->
    cmkit:jsone(Data);

encoded_body(_, Data) ->
    Data.

headers_with_auth(none, Headers) -> Headers;
headers_with_auth(Auth, Headers) ->
    [{"Authorization", "Basic " ++ Auth}|Headers].

auth(User, Passwd) ->
    base64:encode_to_string(
      binary_to_list(<<User/binary,
                       <<":">>/binary,
                       Passwd/binary>>)).

url(Url, Params) when size(Params) == 0  ->
    binary_to_list(Url);

url(Url, Params) ->
    Qs = qs(maps:to_list(Params)),
    binary_to_list(<<Url/binary, <<"?">>/binary, Qs/binary>>).

qs(Qs) ->
    qs(Qs, first, []).

qs([], _, Qs) ->
    cmkit:bin_join(lists:reverse(Qs), <<>>);

qs([{K,V}|Remaining], first, Qs) ->
    KBin = cmkit:to_bin(K),
    VBin = cmkit:to_bin(V),
    qs(Remaining, others, [<<KBin/binary, <<"=">>/binary, VBin/binary>>|Qs]);

qs([{K,V}|Remaining], others, Qs) ->
    qs(Remaining, others, [<< <<"&">>/binary, K/binary, <<"=">>/binary, V/binary>>|Qs]).

url(Scheme, Host, Port, Path) ->
    S = cmkit:to_bin(Scheme),
    H = cmkit:to_bin(Host),
    P = cmkit:to_bin(Port),
    Pa = cmkit:to_bin(Path),
    <<S/binary, <<"://">>/binary, H/binary, <<":">>/binary, P/binary, Pa/binary>>.

