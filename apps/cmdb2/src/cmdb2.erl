-module(cmdb2).
-export([
         reset/1,
         put/2,
         all/3,
         first/4
        ]).

reset(Name) -> 
    cmdb2_util:reset(cmdb2_config:storage(Name), Name).

put(Name, Entries) -> 
    cmdb2_util:put(cmdb2_config:storage(Name), Name, Entries).

all(Name, S, P) -> 
    cmdb2_util:get(cmdb2_config:storage(Name), Name, S, P).

first(Name, S, P, O) ->
    cmdb2_util:get(cmdb2_config:storage(Name), Name, S, P, O).



