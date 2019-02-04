-module(cmdb).
-export([
         reset/1,
         put/2,
         insert/2,
         merge/4,
         merge/5,
         get/2,
         get/3,
         get/4,
         get/5
        ]).

reset(Name) -> 
    cmdb_util:write(Name, reset).

put(Name, Entries) -> 
    cmdb_util:write(Name, {put, Entries}).

insert(Name, Entries) -> 
    cmdb_util:write(Name, {insert, Entries}).

merge(Name, S, Match, Merge) ->
    cmdb_util:write(Name, {merge, S, Match, Merge}).

merge(Name, S, P, Match, Merge) ->
    cmdb_util:write(Name, {merge, S, P, Match, Merge}).

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
