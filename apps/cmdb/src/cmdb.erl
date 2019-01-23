-module(cmdb).
-export([
         reset/1,
         del/2,
         insert/2,
         put/2,
         put_del/3,
         get/2,
         get/3,
         get/4,
         between/5,
         map/4,
         map/5,
         pipeline/2
        ]).

reset(Name) -> 
    cmdb_util:reset(cmdb_config:storage(Name), Name).

del(Name, Keys) ->
    put_del(Name, [], Keys).

insert(Name, Entries) -> 
    cmdb_util:insert(cmdb_config:storage(Name), Name, Entries).

put(Name, Entries) -> 
    put_del(Name, Entries, []).

put_del(Name, ValuesToAdd, KeysToDelete) ->
    cmdb_util:put_del(cmdb_config:storage(Name), Name, ValuesToAdd, KeysToDelete).

get(Name, S, P) -> 
    cmdb_util:inspect(Name, S, P).

get(Name, S) -> 
    cmdb_util:inspect(Name, S).

get(Name, S, P, O) ->
    cmdb_util:inspect(Name, S, P, O).

between(Name, S, P, O1, O2) ->
    EndFun = fun({S0, P0, O0, H, T}, V) when S0 =:= S andalso
                                             P0 =:= P andalso
                                             O0 < O2 -> {ok, {S0, P0, O0, H, T, V}};
                (_, _) -> stop
             end,

    cmdb_util:fold(Name, {S, P, O1, 0, 0}, EndFun).

map(Name, S, Match, Merge) ->
    cmdb_util:map(cmdb_config:storage(Name), Name, S, Match, Merge).

map(Name, S, P, Match, Merge) ->
    cmdb_util:map(cmdb_config:storage(Name), Name, S, P, Match, Merge).

pipeline(Name, P) ->
    cmdb_util:pipeline(cmdb_config:storage(Name), Name, P).
