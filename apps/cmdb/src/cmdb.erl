-module(cmdb).
-export([
         reset/1,
         put/2,
         all/3,
         first/4
        ]).

reset(Name) -> 
    cmdb_util:reset(cmdb_config:storage(Name), Name).

put(Name, Entries) -> 
    cmdb_util:put(cmdb_config:storage(Name), Name, Entries).

all(Name, S, P) -> 
    cmdb_util:get(cmdb_config:storage(Name), Name, S, P).

first(Name, S, P, O) ->
    cmdb_util:get(cmdb_config:storage(Name), Name, S, P, O).



