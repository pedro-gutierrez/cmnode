-module(cmdb).
-export([
         ping/1,
         write/2,
         write/3,
         read/2,
         backup/1,
         restore/2,
         buckets/0
        ]).

buckets() ->
    [ #{ name => cmkit:to_atom(Name),
         storage =>  cmkit:to_atom(Storage),
         hosts => Hosts } 
      || {ok, #{ <<"name">> := Name,
            <<"spec">> := #{ <<"hosts">> := Hosts,
                             <<"storage">> := Storage 
                           }}
          } <- cmyamls:of_type(bucket) ].

ping(Host) ->
    case cmkit:node_for_host(Host) of 
        {ok, N} -> 
            timer:tc(gen_statem, call, [{cmdb_cloud, N}, ping]) ;
        Other  ->
            Other
    end.


write(Db, Pairs) -> 
    in( node_for(Db), Db, {put, Pairs}).

write(Db, K, V) ->
    in( node_for(Db), Db, {put, K, V}).

read(Db, K) ->
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
