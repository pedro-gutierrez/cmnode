-module(cmstore_util).
-export([stores/0,
         init/1,
         open/1,
         reset/1,
         write/2,
         index/1,
         read/2
        ]).
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
            open(Store);
        {error, E} ->
            cmkit:warning({cmstore, Name, open, E}),
            {error, E}
    end.

opts(#{ name := Store }) ->
    File = filename:join([cmkit:data(), atom_to_list(Store)]),
    [{file, File}].

init(Name) ->
    case sqlite3:table_info(Name, log) of
        table_does_not_exist ->
            ok = sqlite3:create_table(Name, log, [{app, text, not_null},
                                                  {kind, text, not_null},
                                                  {id, text, not_null},
                                                  {event, text, not_null},
                                                  {rel, text},
                                                  {other, text},
                                                  {timestamp, integer, not_null},
                                                  {node, text, not_null},
                                                  {data, text, not_null}]),
            [_|_] = sqlite3:sql_exec(Name, "PRAGMA journal_mode = WAL;"),
            ok = sqlite3:sql_exec(Name, "PRAGMA synchronous = NORMAL;"),
            ok = index(Name),
            ok;
        [_|_] ->
            ok;
        {error, _, E} ->
            cmkit:danger({cmstore, init, Name, E}),
            {error, E}
    end.

drop(Name) ->
    sqlite3:drop_table(Name, log).

reset(Name) ->
    ok = drop(Name),
    ok = init(Name),
    ok.


index(Name) ->
    ok = sqlite3:sql_exec(Name, "CREATE INDEX log_kind_id ON log (app, kind, id)"),
    ok = sqlite3:sql_exec(Name, "CREATE INDEX log_kind_event ON log (app, kind, event)"),
    ok = sqlite3:sql_exec(Name, "CREATE INDEX log_rel_other ON log (app, rel, other)"),
    ok = sqlite3:sql_exec(Name, "CREATE INDEX log_kind_id_event ON log (app, kind, id, event)").


write(Name, Entries) when is_list(Entries) ->
    N = cmkit:to_bin(node()),
    T = cmkit:micros(),
    Entries2 = lists:map(fun(E) ->
                                 entry(E,N,T)
                         end, Entries),
    case sqlite3:write_many(Name, log, Entries2) of 
        [ok|_] -> ok;
        Other ->
            Other
    end;

write(Name, E) when is_map(E) ->

    N = cmkit:to_bin(node()),
    T = cmkit:micros(),
    case sqlite3:write(Name, log, entry(E, N, T)) of 
        {rowid, _} -> ok;
        Other ->
            Other
    end.


entry(#{ app := A, 
         kind := K,
         id := Id,
         event := Ev,
         rel := R,
         other := O,
         data := D }, N, T) ->
    [{app, A},
     {kind, K},
     {id, Id},
     {event, Ev},
     {rel, R},
     {other, O},
     {timestamp, T},
     {node, N},
     {data, cmkit:jsone(D)}];

entry(#{ app := _, 
         kind := _,
         id := _,
         event := _,
         data := _} = Spec, N, T) ->
    entry(Spec#{ 
                 rel => null, 
                 other => null}, N, T).

ref(Name, #{ app := A,
             kind := K,
             id := Id,
             event := Ev }) ->
    ?REF(Name, "SELECT app, kind, id, event, rel, other, timestamp, node, data "
         "FROM log "
         "WHERE app = ? "
         "AND kind = ? "
         "AND id = ? "
         "AND event = ? " 
         "ORDER by timestamp ASC", [A, K, Id, Ev]);

ref(Name, #{ app := A, 
             kind := K,
             id := Id }) ->
    ?REF(Name, "SELECT app, kind, id, event, rel, other, timestamp, node, data "
         "FROM log "
         "WHERE app = ? "
         "AND kind = ? "
         "AND id = ? "
         "ORDER by timestamp ASC", [A, K, Id]);

ref(Name, #{ app := A,
             kind := K,
             event := Ev }) ->
    ?REF(Name, "SELECT app, kind, id, event, rel, other, timestamp, node, data "
         "FROM log "
         "WHERE app = ? "
         "AND kind = ? "
         "AND event = ? "
         "ORDER by timestamp ASC", [A, K, Ev]);

ref(Name, #{ app := A, 
             rel := R, 
             other := O }) ->
    ?REF(Name, "SELECT app, kind, id, event, rel, other, timestamp, node, data "
         "FROM log "
         "WHERE app = ? " 
         "AND rel = ? "
         "AND other = ? " 
         "ORDER by timestamp ASC", [A, R, O]);

ref(Name, #{ app := A, 
             kind := K }) ->
    ?REF(Name, "SELECT app, kind, id, event, rel, other, timestamp, node, data "
         "FROM log "
         "WHERE app = ? " 
         "AND kind = ? "
         "ORDER by timestamp ASC", [A, K]).

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

decode({A, K, Id, Ev, R, O, T, N, D}) ->
    {ok, Data} = cmkit:jsond(D), 
    #{ app => A,
       kind => K,
       id => Id,
       event => Ev,
       rel => R, 
       other => O,
       timestamp => T,
       node => N,
       data => Data }.
