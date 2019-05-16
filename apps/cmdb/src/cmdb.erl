-module(cmdb).
-export([
         reset/1,
         restart/1,
         put/2,
         delete/2,
         insert/2,
         merge/4,
         merge/5,
         merge/6,
         match/3,
         match/4,
         get/1,
         get/2,
         get/3,
         get/4,
         get/5
        ]).

reset(Name) -> 
    cmdb_util:write(Name, reset).

restart(Name) -> 
    cmdb_util:write(Name, restart).

put(Name, Entries) -> 
    cmdb_util:write(Name, {put, Entries}).

delete(Name, Entries) -> 
    cmdb_util:write(Name, {delete, Entries}).

insert(Name, Entries) -> 
    cmdb_util:write(Name, {insert, Entries}).

merge(Name, S, V, Merge) ->
    cmdb_util:write(Name, {merge, S, V, Merge}).

merge(Name, S, P, V, Merge) ->
    cmdb_util:write(Name, {merge, S, P, V, Merge}).

merge(Name, S, P, O, V, Merge) ->
    cmdb_util:write(Name, {merge, S, P, O, V, Merge}).

match(Name, S, V) ->
    cmdb_util:read(Name, {S}, V).

match(Name, S, P, V) ->
    cmdb_util:read(Name, {S, P}, V).

get(Name) ->
    cmdb_util:read(Name).

get(Name, Keys) when is_list(Keys) -> 
    cmdb_util:read(Name, Keys);

get(Name, S) -> 
    cmdb_util:read(Name, {S}).

get(Name, S, P) -> 
    cmdb_util:read(Name, {S, P}).

get(Name, S, P, O) ->
    cmdb_util:read(Name, {S, P, O}).

get(Name, S, P, O1, O2) ->
    cmdb_util:read(Name, {S, P, O1, O2}).
