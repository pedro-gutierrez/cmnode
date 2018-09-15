-module(cmdb).
-export([
         reset/1,
         put/2,
         get/2,
         get/3,
         get/4
        ]).

reset(Name) -> 
    cmdb_util:reset(cmdb_config:storage(Name), Name).

put(Name, Entries) -> 
    cmdb_util:put(cmdb_config:storage(Name), Name, Entries).

get(Name, S, P) -> 
    merge(cmdb_util:get(cmdb_config:storage(Name), Name, S, P)).

get(Name, S) -> 
    merge(cmdb_util:get(cmdb_config:storage(Name), Name, S)).

get(Name, S, P, O) ->
    merge(cmdb_util:get(cmdb_config:storage(Name), Name, S, P, O)).

merge({ok, Entries}) -> {ok, cmdb_util:merge(Entries)};
merge(Other) -> Other.
