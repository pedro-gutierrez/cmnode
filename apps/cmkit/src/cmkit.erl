-module(cmkit).
-export([is_true/1, log/1, log_fun/1, print/2, home/0, env/1, etc/0, data/0, data/1, data/2, assets/0, asset/1, yamls/0, yamls/1, yamls/2, yaml/1, files/2, config/2, config/3, err/1, fmt/2, fmt_named/2, xmld/1, jsone/1, jsone/2, jsond/1, yamld/1, millis_since/1, micros_since/1, elapsed/1, seconds/0, micros/0, millis/0, now/0, uuid/0, ret/1, child_spec/2, child_spec/3, child_spec/4, child_spec/5, worker_child_specs/1, worker_child_spec/1, parse/2, diff_mins/2, diff_secs/2, mins_since/1, match_map/2, search_map/2, search_map/3, implements/2, lower_bin/1, list_without/2, to_float/2, to_number/1, to_number/2, bin_to_number/1, distinct/1, ip_str/1, to_atom/1, to_bin/1, sname/0, sname/1, node_names/2, node_name/2, node_host/1, node_host_short/1,  hosts_to_nodes/1, node_for_host/1, node_for_host/2, node_for_host/3, intersection/2, closest_node/1, uniconvert/1, map_join/3, bin_join/1, bin_join/2, bin_split/2, bin_trim/1, to_list/1, to_list/3, date_as_map/1, format_date/1, format_date/2, parse_date/1, fmt_date/0, mkdirp/1, host/0, value_at/2, is_email/1, has_all_keys/2, watch/1, to_lower/1, hash/1, url/3, url/1, print/3, success/1, danger/1, warning/1, to_millis/1, find_by/3, top/2, is_string/1, read_file/1, tar/2, prefix/2, file_info/1, stream_error/4, stream_file/1, printable/1, printable/2, cast/4, encrypt/2, decrypt/2, hex/1, reregister/2, is_json/1, replace/3, merge/2, app_env/2, set_app_env/3, app_env/3, set_app_env/4, capitalize/1, is_application_started/1, ipv4/0, await/2, map_from/2, map_from/3]).
-include_lib("kernel/include/file.hrl").
-define(NOT_FOUND, {error, not_found}).

log(Data)->    
    io:format("[LOG] ~P~n", [Data, 30]).

log_fun(#{ debug := Debug }) ->
    log_fun(Debug);

log_fun(V) ->
    case is_true(V) of 
        true ->
            fun log/1;
        false ->
            fun(_) -> ok end
    end.

is_true(true) -> true;
is_true(<<"true">>) -> true;
is_true(yes) -> true;
is_true(_) -> false.

print(Pattern, Args, Color) ->
    Str = lists:flatten(to_list(fmt(Pattern, Args))),
    io:format("~s~n", [ color:Color(Str)]). 

print(Term, Sev) when is_binary(Term) ->
    print("[LOG] ~P~n", [Term, 20], Sev);

print(Term, Sev) ->
    print("[LOG] ~p~n", [Term], Sev).

success(Term) ->
    print(Term, green).

danger(Term) ->
    print(Term, red).

warning(Term) ->
    print(Term, yellow).

home() -> env("CMHOME").
etc() -> env("CMETC", filename:join([home(), "etc"])).
assets() -> env("CMASSETS", filename:join([home(), "assets"])).
asset(Name) -> filename:join([assets(), Name]).
data() -> env("CMDATA", filename:join([home(), "data"])).
data(Name) -> filename:join([data(), to_list(Name)]).
data(Name, Item) -> filename:join([data(), to_list(Name), to_list(Item)]).

env(Key) -> os:getenv(Key).
env(Key, Default) -> os:getenv(Key, Default).

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
    yamld(uniconvert(Bin));    

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

xmld(Str) when is_list(Str) ->
    try  
        {Element, _} = xmerl_scan:string(Str, [{space, normalize}]),
        [Clean] = xmerl_lib:remove_whitespace([Element]),
        Clean2 = xmerl_lib:simplify_element(Clean),
        {ok, Clean2}
    catch
        _:_ ->
            {error, Str}
    end.

jsond(Bin) when is_binary(Bin) ->
    try 
        case jsone:decode(Bin, [{object_format, map}]) of 
            B when is_binary(B) ->
                {error, Bin};
            Other ->
                {ok, Other}
        end
    catch
        _:_ -> 
            {error, Bin}
    end;

jsond(Str) when is_list(Str) ->
    jsond(uniconvert(Str)).

jsone(Term) ->
    jsone:encode(Term, [native_utf8,
                        {float_format, [{decimals, 32}, compact]}]).

jsone(Term, Opts) -> 
    jsone:encode(Term, Opts).


err(Reason) ->
    {error, #{reason => Reason}}.

fmt(Format, Args) when is_list(Format) ->
    erlang:iolist_to_binary(io_lib:format(xmerl_ucs:to_utf8(Format), Args ));

fmt(Format, Args) when is_binary(Format) ->
    fmt(binary_to_list(Format), Args).

fmt_named(Pattern, Args) when is_list(Pattern) ->
    Regex = "{{(\s*)(.*?)(\s*)}}",
    Parts = re:split(Pattern, Regex),
    fmt_named_replace(Parts, Args);

fmt_named(Pattern, Args) when is_binary(Pattern) ->
    fmt_named(binary_to_list(Pattern), Args).

fmt_named_replace(Parts, Args) ->
    fmt_named_replace(Parts, Args, default, <<>>, []).

fmt_named_replace([], _, default, _, Out) ->
    {ok, bin_join(lists:reverse(Out))};

fmt_named_replace([<<>>|Rem], Args, default, Tmp, Out) ->
    fmt_named_replace(Rem, Args, start_param, Tmp, Out);

fmt_named_replace([Other|Rem], Args, default, Tmp, Out) ->
    fmt_named_replace(Rem, Args, default, Tmp, [Other|Out]);


fmt_named_replace([], _, start_param, _, Out) ->
    {ok, bin_join(lists:reverse(Out))};

fmt_named_replace([<<>>|Rem], Args, start_param, Tmp, Out) ->
    fmt_named_replace(Rem, Args, start_param, Tmp, Out);

fmt_named_replace([ParamName|Rem], Args, start_param, _, Out) ->
    fmt_named_replace(Rem, Args, end_param, ParamName, Out);

fmt_named_replace([<<>>|Rem], Args, end_param, Tmp, Out) ->
    Key = cmkit:to_atom(Tmp),
    case value_at(Key, Args) of 
        undef ->
            {error, #{ reason => missing_param_value,
                       params => maps:keys(Args), 
                       param => Tmp }};
        V ->
            fmt_named_replace(Rem, Args, default, <<>>, [V|Out])
    end;

fmt_named_replace([Other|_], _, end_param, Tmp, _) ->
    {error, #{ reason => unexpected_token,
               param => Tmp,
               info => Other }}.

elapsed(Since) ->
    micros() - Since.

micros_since(Since) ->
    micros() - Since.

millis_since(Millis) ->
    millis() - Millis.

seconds() ->
    erlang:system_time(second).

micros() ->
    erlang:system_time(microsecond).

millis() -> 
    erlang:system_time(millisecond).

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

distinct([])    -> [];
distinct([H|T]) -> [H | [X || X <- distinct(T), X /= H]].

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

to_atom(Str) when is_list(Str) ->
    to_atom(to_bin(Str));

to_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, latin1);

to_atom(A) when is_atom(A) ->
    A.

sname() -> sname(node()).

sname(N) -> 
    [Sname, _] = binary:split(to_bin(N), <<"@">>),
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
    node_for_host( <<"cmnode">>, H, Nodes).

node_for_host(Name, H, Nodes) ->
    HostBin = to_bin(H),
    Match = lists:filter(fun(N) -> 
                                 node_host_short(N) =:= HostBin
                         end, Nodes),
    case Match of 
        [Node] -> {ok, Node};
        [] -> {error, not_found};
        LocalNodes -> 
            case node_with_sname(Name, LocalNodes) of 
                not_found -> {error, not_found};
                Node -> {ok, Node}
            end
    end.

node_with_sname(_, []) -> not_found;
node_with_sname(Name, [N|Rem]) -> 
    case sname(N) of 
        Name -> N;
        _ -> node_with_sname(Name, Rem)
    end.

hosts_to_nodes([], _, Found, NotFound) ->
    {distinct(Found), distinct(NotFound)};



hosts_to_nodes([H|Rem], Nodes, Found, NotFound) ->
    case node_for_host(H) of
        {ok, Node} -> 
            hosts_to_nodes(Rem, Nodes, [Node|Found], NotFound);
        {error, not_found} -> 
            hosts_to_nodes(Rem, Nodes, Found, [H|NotFound])
    end.

node_name(N, H) ->
    Name = case cmkit:bin_split(N, <<"@">>) of
               [N] ->
                   <<N/binary, "@", H/binary>>;
               [_, _] ->
                   N
           end,
    cmkit:to_atom(Name).

node_names(Names, H) ->
    lists:map(fun(N) ->
                      cmkit:node_name(N, H)
              end, Names).

closest_node(Nodes) -> 
    case lists:member(node(), Nodes) of
        true ->
            node();
        false ->
            lists:nth(rand:uniform(length(Nodes)), Nodes)
    end.

uniconvert(String) when is_list(String) ->
    unicode:characters_to_binary(String, utf8);

uniconvert(Bin) when is_binary(Bin) -> 
    unicode:characters_to_list(Bin).

                                                %  try xmerl_ucs:from_utf8(String) of
                                                %    _ ->
                                                %      list_to_binary(String)
                                                %  catch
                                                %    exit:{ucs,{bad_utf8_character_code}} ->
                                                %      list_to_binary(xmerl_ucs:to_utf8(String))
                                                %  end.

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

to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
    (calendar:datetime_to_gregorian_seconds(
       {{Year,Month,Day},{Hours,Minutes,Seconds}}
      ) - 62167219200)*1000000.

date_as_map({{Year, Month, Day}, {Hour, Minute, Second}}=D) -> 
    #{ unix => to_timestamp(D),
       year => Year,
       month => Month,
       day => Day,
       hour => Hour,
       minute => Minute,
       second => Second}.

format_date(Date) -> 
    format_date(Date, iso8601).

format_date(#{ year := Y, month := M, day := D,
               hour := H, minute := Min, second := Secs }, iso8601) -> 
    try 
        to_bin(iso8601:format({{Y, M, D}, {H, Min, Secs}}))
    catch 
        _:_ -> 
            invalid
    end;

format_date(#{ year := Y, month := M, day := D} , yyyymmdd) -> 
    try 
        to_bin(lists:flatten(io_lib:format("~4..0w~2..0w~2..0w",[Y,M,D])))
    catch
        _:_ ->
            invalid
    end;

format_date(#{ year := Y, month := M, day := D, 
               hour := H, minute := Min, second := Secs} , yyyymmddhhmmss) -> 
    try 
        to_bin(lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Y,M,D, H, Min, Secs])))
    catch
        _:_ ->
            invalid
    end;

format_date(_, _) ->
    invalid.

parse_date(Date) -> 
    try
        D2 = binary:replace(Date, <<" ">>, <<"T">>, [global]),
        date_as_map(iso8601:parse(to_bin(D2)))
    catch
        error:badarg ->
            invalid
    end.


to_millis({Y, M, D, H, Min, Sec}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D},{H, Min, Sec}})*1000.

mkdirp(Dir) ->
    filelib:ensure_dir(Dir ++ "/").


value_at(Key, Map) when is_atom(Key) and is_map(Map) ->
    case maps:get(Key, Map, undef) of
        undef ->
            maps:get(to_bin(Key), Map, undef);
        V -> 
            V
    end;

value_at(Key, Map) when is_binary(Key) and is_map(Map) ->
    case maps:get(Key, Map, undef) of 
        undef -> 
            maps:get(to_atom(Key), Map, undef);
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

file_info(Filename) ->
    case file:read_file_info(Filename) of 
        {ok, #file_info{ size = Size,
                         type = Type }} -> {ok, #{ size => Size,
                                                   type => Type,
                                                   filename => Filename }};
        {error, enoent} -> not_found;
        Other -> 
            Other
    end.
        
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

stream_file(#{ stream := Stream,
               path := Filename, 
               bytes := Bytes,
               context := #{ data := Ctx,
                             callback := Cb }}) -> 
    case file:read_file_info(Filename) of
          {ok, #file_info{ size = Size,
                           type = Type }} -> 
            cmkit:log({cmkit, stream_file, Size, Type}),
            case file:open(to_list(Filename), [binary, read, raw]) of 
                {ok, Device} -> 
                    Data = #{ stream => Stream,
                              event => start,
                              data => #{ status => 200,
                                         headers => #{ <<"content-length">> => Size }}},
                    stream_event(Cb, Ctx, Data),
                    read_bytes(Device, Bytes, Stream, Ctx, Cb); 
                {error, Error} -> 
                    Status = status_code_from_file_error(Error),
                    stream_error(Stream, Status, Ctx, Cb)
            end;
        {error, Error} ->
            Status = status_code_from_file_error(Error),
            stream_error(Stream, Status, Ctx, Cb)
    end.

status_code_from_file_error(enoent) -> 404;
status_code_from_file_error(_) -> 500.

stream_event({M, F}, Ctx, Data) -> 
    M:F(maps:merge(Ctx, Data));

stream_event({N, M, F}, Ctx, Data) -> 
    rpc:call(N, M, F, [maps:merge(Ctx, Data)]).

stream_error(Stream, Status, Ctx, Cb) -> 
    Data = #{ stream => Stream,
              event => error,
              data => #{ status => Status,
                         headers => #{ <<"content-length">> => 0 }}},
    stream_event(Cb, Ctx, Data).

read_bytes(Device, Bytes, Stream, Ctx, Cb) -> 
    case file:read(Device, Bytes) of 
        {ok, Data} ->
            stream_event(Cb, Ctx, #{ stream => Stream,
                                     event => data,
                                     data => Data }),
            read_bytes(Device, Bytes, Stream, Ctx, Cb); 
        eof ->
            stream_event(Cb, Ctx, #{ stream => Stream,
                                     event => 'end',
                                     data => #{} }),
            ok = file:close(Device);
        {error, Error} -> 
            Status = status_code_from_file_error(Error),
            stream_error(Stream, Status, Ctx, Cb)
    end.
       
tar(Filename, Dir) ->
    Cmd = fmt("cd ~s; tar czvf ~s .", [Dir, Filename]),
    case sh:oneliner(to_list(Cmd)) of 
        {done, 0, _} -> 
            ok;
        {done, _, Error} -> 
            {error, Error}
    end.

prefix(String, Prefix) ->
    case bin_split(to_bin(String), to_bin(Prefix)) of 
        [<<>>, Suffix] -> Suffix;
        _ -> nomatch
    end.

printable(Term) -> printable(128, Term).

printable(BinSize, List) when is_list(List) ->
    case length(List) > BinSize of
        true ->
            case is_string(List) of 
                true -> 
                    lists:sublist(List, BinSize);
                false ->
                    SL = lists:map(fun(T) ->
                                           printable(BinSize, T)
                                   end, List),
                    lists:sublist(SL, BinSize)
            end;
        false -> 
            lists:map(fun(T) -> 
                              printable(BinSize, T)
                      end, List)
    end;

printable(BinSize, Map) when is_map(Map) ->
    maps:fold(fun(K, Pid, Acc) when is_pid(Pid) -> 
                      Acc#{ K => <<"(omitted pid)">>} ;
                 (K, V, Acc) ->
                      Acc#{ K => printable(BinSize, V) }
              end, #{}, Map);

printable(MaxSize, Bin) when is_binary(Bin) ->
    Size = size(Bin),
    case Size > MaxSize of 
        true -> 
            BinSize = to_bin(size(Bin)),
            <<"(Omitted binary of size ", BinSize/binary ,")">>;
        false -> Bin
    end;

printable(_, V) -> V.

cast([], _, _, _) -> ok;
cast([N|Rem], M, F, Args) ->
    rpc:cast(N, M, F, Args),
        cast(Rem, M, F, Args).

encrypt(Key, Value) -> 
    BinKey = binary:encode_unsigned(list_to_integer(to_list(Key), 16)),
    IV = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    PaddedValue = pad(Value),
    encrypt(BinKey, IV, PaddedValue).

decrypt(Key, Value) -> 
    BinKey = binary:encode_unsigned(list_to_integer(to_list(Key), 16)),
    IV = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    case decrypt(BinKey, IV, Value) of 
        {ok, Dec} -> 
            {ok, unpad(Dec)};
        error -> 
            error
    end.

encrypt(Key, IV, Value) ->
    try 
        {ok, jose_jwa_aes:block_encrypt({aes_cbc, 256}, Key, IV, Value)}
    catch
        _:_ ->
            error
    end.

decrypt(Key, IV, Value) -> 
    try 
        {ok, jose_jwa_aes:block_decrypt({aes_cbc, 256}, Key, IV, Value)}
    catch
        _:_ ->
            error
    end.

pad(Str) ->
    Bin = to_bin(Str),
    Rest = byte_size(Bin) rem 16,
    Bytes = 16 - Rest,
    Extra = binary:copy(<<Bytes>>, Bytes),
    <<Bin/binary, Extra/binary>>.

unpad(<<>>) ->
    <<>>;

unpad(Bin) ->
    Length = byte_size(Bin) - binary:last(Bin),
    case Length of
        N when N < 0 ->
            Bin;
        _ ->
            binary:part(Bin, 0, Length)
    end.

hex(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>;

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

reregister(Name, Pid) ->
    case erlang:whereis(Name) of 
        undefined -> 
            true = register(Name, Pid),
            ok;
        _ -> 
            unregister(Name),
            true = register(Name, Pid),
            ok
    end.

is_json(<<"application/json">>) -> true;
is_json(<<"application/json", _/binary>>) -> true;
is_json(_) -> false.

replace(Source, Search, Replace) ->
    binary:replace(Source, Search, Replace, [global]).

merge_maps(Keys, M1, M2) ->
    merge_maps(Keys, M1, M2, #{}).

merge_maps([], _, _, Res) -> Res;
merge_maps([K|Rem], M1, M2, Res) ->
    V = merge(maps:get(K, M1), maps:get(K, M2)),
    merge_maps(Rem, M1, M2, Res#{ K => V }).

merge(M, M) when is_map(M) ->
    M;

merge(MA, MB) when is_map(MA) andalso is_map(MB) ->
    KeySetA = sets:from_list(maps:keys(MA)),
    KeySetB = sets:from_list(maps:keys(MB)),
    CommonKeys = sets:to_list(sets:intersection(KeySetA, KeySetB)),
    M0 = maps:without(CommonKeys, MA),
    M1 = maps:merge(M0, maps:without(CommonKeys, MB)),
    M2 = maps:merge(M1, merge_maps(CommonKeys, MA, MB)),
    M2;

merge(L1, L2) when is_list(L1) andalso is_list(L2) ->
    L1++L2;

merge(A, A) -> A;

merge(_, B) -> B.

app_env(App, Ns, K) when is_atom(K) ->
    case application:get_env(App,{Ns, K}) of 
        undefined -> ?NOT_FOUND;
        V -> V
    end;

app_env(App, Ns, K) when is_binary(K) ->
    app_env(App, Ns, cmkit:to_atom(K)).

app_env(App,K) when is_binary(K) ->
    app_env(App, cmkit:to_atom(K));

app_env(App,K) when is_atom(K) ->
    case application:get_env(App,K) of 
        undefined -> ?NOT_FOUND;
        V -> V
    end;

app_env(App,K) when is_binary(K) ->
    app_env(App, cmkit:to_atom(K)).

set_app_env(App, K, V) ->
    application:set_env(App, cmkit:to_atom(K), V, [{persistent, true}]).

set_app_env(App, Ns, K, V) ->
    application:set_env(App, {cmkit:to_atom(Ns), cmkit:to_atom(K)}, V, [{persistent, true}]).


capitalize(V) when is_binary(V) ->
    Str = cmkit:uniconvert(V),
    First = string:slice(Str, 0, 1),
    Rest = string:slice(Str, 1, length(Str)-1),
    FirstCapBin = to_bin(string:to_upper(First)),
    RestBin = to_bin(Rest),
    <<FirstCapBin/binary, RestBin/binary>>.

is_application_started(App) ->
    case [A || {A, _, _ } <- application:which_applications(), A =:= App] of 
        [] -> false;
        [App] -> true
    end.

ipv4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).


await(_, #{ retries := 0 }) -> false;

await(Condition, #{ retries := Left,
                    sleep := Sleep } = Config) ->
    case Condition() of 
        true ->
            true;
        _ ->
            log_await(Config),
            timer:sleep(Sleep),
            await(Condition, Config#{ retries => Left - 1 })
    end.


log_await(#{ debug := true,
             sleep := Sleep,
             retries := Left }) -> 
    cmkit:log({cmkit, sleeping, Sleep, Left});

log_await(_) -> ok.

map_from(Keys, Values) ->
    map_from(Keys, Values, binary, #{}).

map_from(Keys, Values, ValueType) ->
    map_from(Keys, Values, ValueType, #{}).

map_from([], [], _, Map) -> {ok, Map};
map_from([K|Rem], [V|Rem2], ValueType, Map) ->
    case to_type(V, ValueType) of 
        {ok, V2} ->
            map_from(Rem, Rem2, ValueType, Map#{ K => V2 });
        {error, E} ->
            {error, #{ key => K,
                       value => V,
                       type => ValueType,
                       reason => E }}
    end.

to_type(Str, binary) -> 
    {ok, cmkit:to_bin(Str) };
to_type(Str, number) -> 
    case cmkit:to_number(Str) of 
        error ->
            {error, not_a_number};
        Num ->
            {ok, Num}
    end.
