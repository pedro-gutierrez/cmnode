-module(cmstore_util).
-export([stores/0,
         init/1,
         open/1,
         reset/1,
         write/2,
         index/1,
         read/2
        ]).
-define(TABLE, entries).
-define(SCHEMA, [{s, text, not_null},
                 {p, text, not_null},
                 {o, text, not_null},
                 {n, text, not_null},
                 {t, integer, not_null},
                 {v, text, not_null}]).
-define(BY_S, "SELECT s, p, o, v FROM entries WHERE s = ? ORDER by t ASC").
-define(BY_O, "SELECT s, p, o, v FROM entries WHERE o = ? ORDER by t ASC").
-define(BY_P, "SELECT s, p, o, v FROM entries WHERE p = ? ORDER by t ASC").
-define(BY_S_P, "SELECT s, p, o, v FROM entries WHERE s = ? AND p = ? ORDER by t ASC").
-define(BY_S_O, "SELECT s, p, o, v FROM entries WHERE s = ? AND o = ? ORDER by t ASC").
-define(BY_P_O, "SELECT s, p, o, v FROM entries WHERE p = ? AND o = ? ORDER by t ASC").
-define(BY_S_P_O, "SELECT s, p, o, v FROM entries WHERE s = ? AND p = ? AND o = ? ORDER by t ASC").
-define(INDEX_S_P_O, "CREATE INDEX entries_s ON entries (s, p, o)").
-define(REF(Name, Ps, Args), 
        {ok, Ref} = sqlite3:prepare(Name, Ps),
        ok = sqlite3:bind(Name, Ref, Args),
        {ok, Ref}).

stores() ->
    Buckets = [ #{ name => cmkit:to_atom(Name),
                   storage => cmkit:to_atom(Storage),
                   debug => cmkit:to_atom(maps:get(<<"debug">>, Spec, <<"false">>))
                 } || {ok, #{ <<"name">> := Name,
                              <<"spec">> := #{ 
                                  <<"storage">> := Storage } = Spec}} <- cmkit:yamls(store)] ,
    {ok, Buckets}.


open(#{ name := Name} = Store) ->
    case sqlite3:open(Name, opts(Store)) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, _}} ->
            ok = sqlite3:close(Name),
            open(Store)
    end.

opts(#{ name := Store }) ->
    File = filename:join([cmkit:data(), atom_to_list(Store)]),
    [{file, File}].

init(Name) ->
    case sqlite3:table_info(Name, ?TABLE) of
        table_does_not_exist ->
            ok = sqlite3:create_table(Name, ?TABLE, ?SCHEMA),
            [_|_] = sqlite3:sql_exec(Name, "PRAGMA journal_mode = WAL;"),
            ok = sqlite3:sql_exec(Name, "PRAGMA synchronous = NORMAL;"),
            ok = index(Name),
            ok;
        [_|_] ->
            ok
    end.

drop(Name) ->
    sqlite3:drop_table(Name, ?TABLE).

reset(Name) ->
    ok = drop(Name),
    ok = init(Name),
    ok.


index(Name) ->
    ok = sqlite3:sql_exec(Name, ?INDEX_S_P_O).



write(Name, Entries) when is_list(Entries) ->
    N = cmkit:to_bin(node()),
    T = cmkit:micros(),
    Entries2 = lists:map(fun(E) ->
                                 entry(E,N,T)
                         end, Entries),
    case sqlite3:write_many(Name, ?TABLE, Entries2) of 
        [ok|_] -> ok;
        Other ->
            Other
    end;

write(Name, E) when is_map(E) ->

    N = cmkit:to_bin(node()),
    T = cmkit:micros(),
    case sqlite3:write(Name, ?TABLE, entry(E, N, T)) of 
        {rowid, _} -> ok;
        Other ->
            Other
    end.


entry(#{ subject := S,
         predicate := P,
         object := O,
         value := V }, N, T) ->
    [{s, S},
     {p, P},
     {o, O},
     {n, N},
     {t, T},
     {v, cmkit:jsone(V)}].

ref(Name, #{ subject := S,
             predicate := P,
             object := O }) ->
    ?REF(Name, ?BY_S_P_O, [S, P, O]);

ref(Name, #{ subject := S,
             predicate := P }) ->
    ?REF(Name, ?BY_S_P, [S, P]);

ref(Name, #{ subject := S,
             object := O }) ->
    ?REF(Name, ?BY_S_O, [S, O]);

ref(Name, #{ predicate := P,
             object :=  O}) ->
    ?REF(Name, ?BY_P_O, [P, O]);

ref(Name, #{ predicate := P }) ->
    ?REF(Name, ?BY_P, [P]);

ref(Name, #{ subject := S }) ->
    ?REF(Name, ?BY_S, [S]);

ref(Name, #{ object := O }) ->
    ?REF(Name, ?BY_O, [O]).

read(Name, Spec) ->
    {ok, Ref} = ref(Name, Spec),
    collect(Name, Ref).

collect(Name, Ref) ->
    collect(Name, Ref, sqlite3:next(Name, Ref), []).

collect(Name, Ref, done, Res) ->
    ok = sqlite3:finalize(Name, Ref),
    {ok, lists:reverse(Res)};

collect(Name, Ref, R, Res) ->
    collect(Name, Ref, sqlite3:next(Name, Ref), [decode(R)|Res]).

decode({S, P, O, V}) ->
    {ok, Term} = cmkit:jsond(V), 
    #{ subject => S,
       predicate => P,
       object => O,
       value => Term }.
