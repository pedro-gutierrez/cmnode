-module(cmdb).
-export([
         ping/1,
         find/2,
         put/2,
         put_new/2,
         put/3,
         put_new/3,
         get/2,
         backup/1,
         restore/2
        ]).

ping(Host) ->
    case cmkit:node_for_host(Host) of 
        {ok, N} -> 
            timer:tc(gen_statem, call, [{cmdb_cloud, N}, ping]) ;
        Other  ->
            Other
    end.


find(Db, Type) ->
    in( node_for(Db), Db, {find, Type}).

put(Db, Pairs) -> 
    in( node_for(Db), Db, {put, Pairs}).

put_new(Db, Pairs) -> 
    in( node_for(Db), Db, {put_new, Pairs}).

put(Db, K, V) ->
    cmdb:put(Db, [{K, V}]).

put_new(Db, K, V) ->
    cmdb:put_new(Db, [{K, V}]).

get(Db, K) ->
    in( node_for(Db), Db, {get, K}).

backup(Db) -> 
    in( node_for(Db), Db, backup).

restore(Db, Name) -> 
    in( node_for(Db), Db, {restore, Name}).

node_for(Db) -> 
    gen_statem:call({cmdb_cloud, node()}, {node, Db}).

in({ok, N}, Db, Op) ->
    gen_statem:call({Db, N}, Op);

in(_, _, _) ->
    {error, unavailable}.
