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
    cmdb_bucket:put(Bucket, Pairs).

put_new(Bucket, Pairs) -> 
    cmdb_bucket:put_new(Bucket, Pairs).

put(Bucket, K, V) ->
    cmdb_bucket:put(Bucket, [{K, V}]).

put_new(Bucket, K, V) ->
    cmdb_bucket:put_new(Bucket, [{K, V}]).

get(Bucket, K) -> 
    cmdb_util:get(Bucket, K, [node()|nodes()]).

find(Bucket, Type) ->
    cmdb_util:find(Bucket, Type, [node()|nodes()]).

reset(Bucket) ->
    cmdb_util:reset(Bucket, [node()|nodes()]).

backup(_Db) ->  ok.
restore(_Db, _Name) ->  ok.
