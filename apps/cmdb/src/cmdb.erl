-module(cmdb).
-export([
         reset/1,
         del/2,
         put/2,
         get/2,
         get/3,
         get/4,
         map/4
        ]).

reset(Name) -> 
    cmdb_util:reset(cmdb_config:storage(Name), Name).

del(Name, Entries) ->
    cmdb_util:del(cmdb_config:storage(Name), Name, Entries).

put(Name, Entries) -> 
    cmdb_util:put(cmdb_config:storage(Name), Name, Entries).

get(Name, S, P) -> 
    merge(cmdb_util:get(cmdb_config:storage(Name), Name, S, P)).

get(Name, S) -> 
    merge(cmdb_util:get(cmdb_config:storage(Name), Name, S)).

get(Name, S, P, O) ->
    merge(cmdb_util:get(cmdb_config:storage(Name), Name, S, P, O)).

map(Name, S, Match, Merge) ->
    cmdb_util:map(cmdb_config:storage(Name), S, Match, Merge).

merge({ok, Entries}) -> {ok, cmdb_util:merge(Entries)};
merge(Other) -> Other.
