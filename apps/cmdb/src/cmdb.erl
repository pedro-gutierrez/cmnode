-module(cmdb).
-export([info/1,
         find/2,
         put/2,
         put_new/2,
         put/3,
         put_new/3,
         get/2,
         backup/1,
         restore/2,
         reset/1
        ]).

info(Bucket) -> 
    cmdb_util:bucket_info(Bucket).

put(Bucket, Pairs) ->
    N = cmdb_util:replica_count(Bucket),
    put(Bucket, Pairs, [{replicas, N}]).

put(Bucket, Pairs, Opts) -> 
    cmdb_bucket:put(Bucket, Pairs, Opts).

put_new(Bucket, Pairs) ->
    N = cmdb_util:replica_count(Bucket),
    put_new(Bucket, Pairs, [{replicas, N}]).

put_new(Bucket, Pairs, Opts) -> 
    cmdb_bucket:put_new(Bucket, Pairs, Opts).

get(Bucket, K) -> 
    cmdb_util:get(Bucket, K, [node()|nodes()]).

find(Bucket, Type) ->
    cmdb_util:find(Bucket, Type, [node()|nodes()]).

reset(Bucket) ->
    cmdb_util:reset(Bucket, [node()|nodes()]).

backup(_Db) ->  ok.
restore(_Db, _Name) ->  ok.
