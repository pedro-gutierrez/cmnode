-module(cmkit).
-export([log/1, log_fun/1, home/0, env/1, etc/0, assets/0, yamls/0, yamls/1, yamls/2, yaml/1, files/2, config/2, config/3, err/1, fmt/2, jsone/1, jsone/2, jsond/1, yamld/1, now/0, uuid/0, ret/1, child_spec/2, child_spec/3, child_spec/4, child_spec/5, worker_child_specs/1, worker_child_spec/1, parse/2, diff_mins/2, diff_secs/2, mins_since/1, match_map/2, search_map/2, search_map/3, implements/2, lower_bin/1, list_without/2, to_float/2, to_number/1, to_number/2, bin_to_number/1, distinct/1, ip_str/1, to_atom/1, to_bin/1, sname/0, node_host/1, node_host_short/1,  hosts_to_nodes/1, node_for_host/1, intersection/2, closest_node/1, uniconvert/1, map_join/3, bin_join/1, bin_join/2, bin_split/2, bin_trim/1, to_list/1, to_list/3, fmt_date/0, mkdirp/1, host/0, value_at/2, is_email/1, has_all_keys/2, watch/1, to_lower/1, hash/1, url/3, url/1, print/3, success/1, danger/1, warning/1, to_millis/1, find_by/3, top/2, is_string/1, read_file/1, tar/2, prefix/2]).
-include_lib("kernel/include/file.hrl").

log(Data)->
    io:format("[LOG] ~P~n", [Data, 20]).

log_fun(true) -> 
    fun log/1;

log_fun(false) ->
    fun(_) -> ok end.

print(Pattern, Args, Color) ->
    Str = lists:flatten(to_list(fmt(Pattern, Args))),
    io:format("~s~n", [ color:Color(Str)]). 

print(Term, Sev) when is_binary(Term) ->
    print("[LOG] ~P~n", [Term, 20], Sev);

print(Term, Sev) ->
    print("[LOG] ~p~n", [Term], Sev).

success(Term) ->
    print(Term, greenb).

danger(Term) ->
    print(Term, red).

warning(Term) ->
    print(Term, yellow).

home() -> env("CMHOME").
etc() -> filename:join([home(), "etc"]).
assets() -> filename:join([home(), "assets"]).

env(Key) -> os:getenv(Key).

files(Dir, Ext) ->
    filelib:fold_files(Dir, Ext ++ "$", true, fun(Filename, All) ->
                                                      [Filename|All]
                                              end, []).

yaml_files() ->
    files(etc(), ".yml").

yamls() ->
    lists:map(fun yaml/1, yaml_files()).

yaml(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            case yamld(Data) of
                {ok, [Yaml]} -> 
                    {ok, Yaml#{ <<"source">> => File }};
                Other -> Other
            end;
        Other -> Other
    end.


yamls(Type) ->
    TypeBin = to_bin(Type),
    lists:filter(fun({ok, #{ <<"type">> := Type2}}) -> 
                         Type2 =:= TypeBin;
                    (_) -> false end, yamls()).

yamls(Type, Cat) ->
    TypeBin = to_bin(Type),
    CatBin = to_bin(Cat),
    lists:filter(fun({ok, #{ <<"type">> := Type2, <<"category">> := Cat2}}) -> 
                         (Type2 =:= TypeBin) and (Cat2 =:= CatBin) ;
                    (_) -> false end, yamls()).

config(Key, App) ->
    case application:get_env(App, Key) of
        undefined -> undefined;
        {ok, Val} -> Val
    end.

config(Key, App, Default) ->
    case config(Key, App) of 
        undefined -> Default;
        Val -> Val
    end.

yamld(Bin) when is_binary(Bin) ->
    yamld(binary_to_list(Bin));    

yamld(Str) when is_list(Str) ->
    try 
        case yamerl:decode(Str, [{map_node_format, map}, str_node_as_binary]) of 
            [_|_]=Docs ->
                {ok, Docs};
            Other ->
                {error, Other}
        end
    catch
        _:Error ->
            {error, Error}
    end.

jsond(Bin) when is_binary(Bin) ->
    try 
        {ok, jsone:decode(Bin, [{object_format, map}])}
    catch
      _:_ -> 
            {error, Bin}
    end;

jsond(Str) when is_list(Str) ->
    jsond(uniconvert(Str)).

jsone(Term) ->
    jsone:encode(Term, [{float_format, [{decimals, 32}, compact]}]).

jsone(Term, Opts) -> 
    jsone:encode(Term, Opts).


err(Reason) ->
  {error, #{reason => Reason}}.

fmt(Format, Args) when is_list(Format) ->
  erlang:iolist_to_binary(io_lib:format(xmerl_ucs:to_utf8(Format), Args ));

fmt(Format, Args) when is_binary(Format) ->
    fmt(binary_to_list(Format), Args).

now() ->
    erlang:system_time(millisecond).

uuid() ->
  list_to_binary(uuid:to_string(uuid:uuid4())).

ret(R) -> R.

worker_child_specs(Mods) ->
    [ worker_child_spec(M) || M <- Mods ].

worker_child_spec(M) ->
    child_spec(M, worker).

child_spec(M, Type) ->
  child_spec(M, [], Type).

child_spec(M, Args, Type) ->
  child_spec(M, M, Args, Type).

child_spec(Id, M, Args, Type) ->
    child_spec(Id, M, Args, permanent, Type).

child_spec(Id, M, Args, Restart, Type) ->
  {Id, {M, start_link, Args}, Restart, 5000, Type, [M]}.

parse(Spec, Json) ->
  parse_input(Spec, #{}, Json).

parse_input([{Type, K} |Rest], SoFar, Json) ->
  case parse_value(Type, K, Json) of
    invalid -> {errors, #{invalid => K}};
    missing -> {errors, #{missing => K}};
    V -> parse_input(Rest, maps:put(K, V, SoFar), Json)
  end;

parse_input([{Type, K, Default} |Rest], SoFar, Json) ->
  case parse_value(Type, K, Json) of
    invalid -> {errors, #{invalid => K}};
    missing -> parse_input(Rest, maps:put(K, Default, SoFar), Json);
    V -> parse_input(Rest, maps:put(K, V, SoFar), Json)
  end;
  
parse_input([], SoFar, _) -> {ok, SoFar}.

parse_value(Type, [K|Rest], Json) ->
  case maps:is_key(K, Json) of
    false -> missing; 
    true -> 
      parse_value(Type, Rest, maps:get(K, Json))
  end;


parse_value(Type, K, Json) ->
  case maps:is_key(K, Json) of
    false -> missing;
    true ->
      parse_actual(Type, K, Json)
  end.

parse_actual(text, K, Json) ->
  case maps:get(K, Json) of
    <<"">> -> invalid;
    V when is_binary(V) -> V;
    _ -> invalid
  end;

parse_actual(list, K, Json) ->
  case maps:get(K, Json) of
    V when is_list(V) -> V;
    _ -> invalid
  end.

mins_since(T) ->
  diff_mins(T, calendar:universal_time()).

diff_mins(T1, T2) ->
  {D, {H, M, _}} = calendar:time_difference(T1, T2),
  M+60*H+24*60*D.

diff_secs(T1, T2) ->
  60*diff_mins(T1, T2).


match_map([{K, V}|Rem], Map) when is_map(Map) ->
    case maps:is_key(K, Map) of
        false -> false;
        true ->
            case maps:get(K, Map) of 
                V -> 
                    match_map(Rem, Map);
                _ -> false
            end
    end;

match_map([], _) -> true;
match_map(_, _)  -> false.

search_map(Pattern, Fields, Map) when is_map(Map) ->
    search_map(Pattern, maps:with(Fields, Map));

search_map(_, _, _) -> false.

search_map(Pattern, Map) when is_map(Map) ->
    cmkit:log({cmkit, search_map, Pattern, Map}),
    search_list(Pattern, maps:values(Map));

search_map(_,_) -> false.

search_list(_, []) -> false;
search_list(Pattern, [H|T]) when is_binary(H) ->
    case binary:match(lower_bin(H), Pattern) of
        nomatch ->
            search_list(Pattern, T);
        {_, _} -> 
            true
    end;

search_list(Pattern, [_|T]) -> 
    search_list(Pattern, T).


implements(M, Callbacks) ->
    Exports = M:module_info(exports),
    Found = lists:filter(fun(Callback) ->
                lists:member(Callback, Callbacks)
            end, Exports),
    length(Found) == length(Callbacks).


lower_bin(B) ->
    binary:list_to_bin(string:to_lower(binary:bin_to_list(B))).

to_lower(B) ->
    lower_bin(B).

hash(T) -> erlang:phash2(T).

list_without(Map, List) ->
    list_without(Map, [], List).

list_without(_, Out, []) ->
    lists:reverse(Out);

list_without(#{id := Id}=Map, Out, [#{ id := Id}|Rest]) ->
    list_without(Map, Out, Rest);

list_without(Map, Out, [H|Rest]) ->
    list_without(Map, [H|Out], Rest).

bin_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            L2 = re:replace(L, "^-\\.", "-0.", [{return, list}]),
            L3 = re:replace(L2, "^\\.", "0.", [{return, list}]),
            list_to_number(L3, final_float)
    end.

list_to_number(L, final_float) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_number(L, final_int)
    end;

list_to_number(L, final_int) ->
    try list_to_integer(L)
    catch
        error:badarg -> error 
    end.

to_float(Int, Dec) -> 
    to_number(bin_join([ to_bin(Int),
                         to_bin(Dec)], <<".">>)).

to_number(N) when is_number(N) -> N;
to_number(B) when is_binary(B) -> bin_to_number(B);
to_number(L) when is_list(L) -> list_to_number(L).

to_number(B, Default) ->
    try to_number(B)
    catch
        error:badarg -> 
            Default
    end.


distinct(List) ->
    sets:to_list(sets:from_list(List)).

intersection(List1, List2) ->
    sets:to_list(sets:intersection(sets:from_list(List1),
                                  sets:from_list(List2))).

ip_str(Ip) ->
    inet:ntoa(Ip).

to_bin(A) when is_atom(A) ->
    erlang:atom_to_binary(A, latin1);

to_bin(A) when is_binary(A) ->
    A;

to_bin(A) when is_list(A) ->
    erlang:list_to_binary(A);

to_bin(A) when is_integer(A) ->
    list_to_binary(integer_to_list(A));

to_bin({_, _, _, _}=Ip) ->
    to_bin(inet:ntoa(Ip)).

to_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, latin1);

to_atom(A) when is_atom(A) ->
    A.

sname() -> 
    [Sname, _] = binary:split(to_bin(node()), <<"@">>),
    Sname.

host() ->
    node_host(node()).

node_host(Node) ->
    [_, Host] = binary:split(to_bin(Node), <<"@">>),
    Host.

node_host_short(Node) ->
    Host = node_host(Node),
    [Short|_] = binary:split(Host, <<".">>),
    Short.

hosts_to_nodes(Hosts) ->
    hosts_to_nodes(Hosts, [node()|nodes()], [], []).

node_for_host(H) -> 
    node_for_host(H, [node()|nodes()]).

node_for_host(H, Nodes) ->
    Match = lists:filter(fun(N) -> 
                            node_host_short(N) =:= to_bin(H)
                         end, Nodes),
    case Match of 
        [Node] -> {ok, Node};
        [] -> {error, not_found};
        _ -> {error, too_many_nodes}
    end.

hosts_to_nodes([], _, Found, NotFound) ->
    {distinct(Found), distinct(NotFound)};

hosts_to_nodes([H|Rem], Nodes, Found, NotFound) ->
    case node_for_host(H, Nodes) of
        {ok, Node} -> 
            hosts_to_nodes(Rem, Nodes, [Node|Found], NotFound);
        {error, not_found} -> 
            hosts_to_nodes(Rem, Nodes, Found, [H|NotFound])
    end.

closest_node(Nodes) -> 
    case lists:member(node(), Nodes) of
        true ->
            node();
        false ->
            lists:nth(rand:uniform(length(Nodes)), Nodes)
    end.

uniconvert(String) ->
  try xmerl_ucs:from_utf8(String) of
    _ ->
      list_to_binary(String)
  catch
    exit:{ucs,{bad_utf8_character_code}} ->
      list_to_binary(xmerl_ucs:to_utf8(String))
  end.

map_join(Map, Sep1, Sep2) ->
    cmkit:bin_join(maps:fold(fun(K, V, Acc) ->
                                     Kbin = cmkit:to_bin(K),
                                     VBin = cmkit:to_bin(V),
                                     [<<Kbin/binary, Sep1/binary, VBin/binary>>|Acc]
                             end, [], Map), Sep2). 

bin_join(List) when is_list(List) ->
    bin_join(List, <<"">>).

bin_join([], _Sep) ->
    <<>>;
bin_join([Part], _Sep) ->
    to_bin(Part);
bin_join(List, Sep) ->
    lists:foldr(fun (A, B) ->
                        A2 = to_bin(A),
                        if
                            bit_size(B) > 0 -> <<A2/binary, Sep/binary, B/binary>>;
                            true -> A2
                        end
                end, <<>>, List).

bin_split(Bin, Sep) ->
    binary:split(Bin, Sep, [global]).

bin_trim(Bin) ->
    Parts = re:split(Bin, "[(^\\h*)(\\h$)]",[trim]),
    lists:last(Parts).

to_list(L) when is_list(L) -> 
    L;
to_list(B) when is_binary(B) ->
    binary_to_list(B);

to_list(A) when is_atom(A) -> 
    atom_to_list(A);

to_list(M) when is_map(M) ->
    maps:fold(fun(K, V, Out) when is_map(V) ->
                      [{K, to_list(V)}|Out];
                 (K, V, Out) when is_list(V) ->
                      case is_string(V) of 
                          false -> 
                            [{K, lists:map(fun to_list/1, V)}|Out];
                          true -> 
                              [{K, V}|Out]
                      end;
                 (K, V, Out) ->
                      [{K, V}|Out]
              end, [], M).

to_list(Map, KeyTitle, ValueTitle) when is_map(Map) ->
    maps:fold(fun(K, V, Out) ->
                      [#{ KeyTitle => K, 
                         ValueTitle => V }|Out]
              end, [], Map).





localtime() ->
    Now = os:timestamp(),
    calendar:now_to_local_time(Now).

fmt_date() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = localtime(), 
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Year,Month,Day,Hour,Minute,Second])).

to_millis({Y, M, D, H, Min, Sec}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D},{H, Min, Sec}})*1000.

mkdirp(Dir) ->
    filelib:ensure_dir(Dir ++ "/").


value_at(Key, Map) when is_atom(Key) and is_map(Map) ->
    case maps:get(Key, Map, undef) of
        undef ->
            value_at(to_bin(Key), Map);
        V -> 
            V
    end;

value_at(Key, Map) when is_binary(Key) and is_map(Map) ->
    case maps:get(Key, Map, undef) of 
        undef -> 
            case lists:filter(fun(K) ->
                                      Key =:= cmkit:to_bin(K)
                              end, maps:keys(Map)) of 
                [] -> undef;
                [K|_] ->
                    maps:get(K, Map)
            end;
        V -> V
    end.


is_email(Email) ->
    case re:run(Email, [$^|email_re()]++"$", [extended]) of
        nomatch   -> false;
        {match,_} -> true
    end.

email_re() ->
    "(
            (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
        |   ([\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+)*
            )
    )
    @
    (
        (
            ([A-Za-z0-9\\-])+\\.
        )+
        [A-Za-z\\-]{2,}
    )".


has_all_keys([], _)  -> true;
has_all_keys([K|Rem], Map) ->
    case maps:is_key(K, Map) of 
        false -> false;
        true -> has_all_keys(Map, Rem)
    end.

watch(Dir) ->
    {ok, Pid} = fs:start_link(fs_watcher, Dir),
    ok = fs:subscribe(fs_watcher),
    {ok, Pid}.

url(#{ host := Host,
       transport := Transport,
       port := Port,
       path := Path }) ->
    url(Transport, Host, Port, Path).

url(Transport, Port, Path) ->
    url(Transport, localhost, Port, Path).

url(Transport, Host, Port, Path) ->
    TransportBin = cmkit:to_bin(Transport),
    HostBin = cmkit:to_bin(Host),
    PortBin = cmkit:to_bin(Port),
    PathBin = cmkit:to_bin(Path),
    <<TransportBin/binary, 
                     "://", 
                     HostBin/binary, 
                     ":", 
                     PortBin/binary, 
                     PathBin/binary >>.

find_by(_, _, []) -> not_found;
find_by(Key, Value, [Spec|Rem]) -> 
    case maps:is_key(Key, Spec) of 
        false -> find_by(Key, Value, Rem);
        true ->
            case maps:get(Key, Spec) of 
                Value -> {ok, Spec};
                _ -> find_by(Key, Value, Rem)
            end
    end.


top(N, L) ->
    {Top, _} = lists:split(min(N, length(L)), L),
    Top.

is_string(Term) when is_list(Term) ->
    io_lib:printable_unicode_list(Term);

is_string(_) -> false.

read_file(Filename) ->
    case file:read_file_info(Filename) of 
        {ok, #file_info{ size = Size,
                         type = Type,
                         atime = Atime,
                         mtime = Mtime,
                         ctime = Ctime,
                         mode = Mode }} ->
            case file:read_file(Filename) of 
                {ok, Data} ->
                    {ok, #{ data => Data,
                            info => #{ size => Size,
                                        type => Type, 
                                        atime => Atime,
                                        mtime => Mtime,
                                        ctime => Ctime,
                                        mode => Mode }}};
                Other -> Other
            end;
        Other -> Other
    end.
       
tar(Filename, Dir) ->
    Cmd = fmt("cd ~s; tar czvf ~s .", [Dir, Filename]),
    case sh:oneliner(to_list(Cmd)) of 
        {done, _, _} -> ok;
        Other -> Other
    end.

prefix(String, Prefix) ->
    case bin_split(to_bin(String), to_bin(Prefix)) of 
        [<<>>, Suffix] -> Suffix;
        _ -> nomatch
    end.
