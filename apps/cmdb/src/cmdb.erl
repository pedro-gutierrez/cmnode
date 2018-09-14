-module(cmdb).
-export([
         reset/1,
         put/2,
         get/2,
         get/3,
         get/4,
         first/4
        ]).

reset(Name) -> 
    cmdb_util:reset(cmdb_config:storage(Name), Name).

put(Name, Entries) -> 
    cmdb_util:put(cmdb_config:storage(Name), Name, Entries).

get(Name, S, P) -> 
    cmdb_util:get(cmdb_config:storage(Name), Name, S, P).

get(Name, S) -> 
    cmdb_util:get(cmdb_config:storage(Name), Name, S).

get(Name, S, P, O) ->
    cmdb_util:get(cmdb_config:storage(Name), Name, S, P, O).

first(Name, S, P, O) ->
    case get(Name, S, P, O) of 
        {ok, Entries} ->
            {ok, Entries};
        Other ->
            Other
    end.

