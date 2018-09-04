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
    Res = put(Bucket, Pairs, [{replicas, N}]),
    Res.

put(Bucket, Pairs, Opts) -> 
    put(Bucket, Pairs, Opts, cmdb_config:backend(Bucket)).

put(Bucket, Pairs, _, ets) -> 
    case ets:insert(Bucket, Pairs) of
        true ->
            ok;
        Other ->
            Other
    end;

put(Bucket, Pairs, Opts, dets) ->
    cmdb_bucket:put(Bucket, Pairs, Opts).

put_new(Bucket, Pairs) ->
    N = cmdb_util:replica_count(Bucket),
    put_new(Bucket, Pairs, [{replicas, N}]).

put_new(Bucket, Pairs, Opts) -> 
    cmdb_bucket:put_new(Bucket, Pairs, Opts).

get(Bucket, K) -> 
    get(Bucket, K, cmcloud:current_nodes(), cmdb_config:backend(Bucket)).

get(Bucket, K, [_], ets) ->
    case ets:lookup(Bucket, K) of 
        [] -> not_found;
        Items -> {ok, [V || {_, V} <- Items]}
    end;

get(Bucket, K, Nodes, ets) ->
    cmdb_util:get(Bucket, K, Nodes);

get(Bucket, K, Nodes, dets) ->
    cmdb_util:get(Bucket, K, Nodes).

find(Bucket, Type) ->
    cmdb_util:find(Bucket, Type, cmcloud:current_nodes()).

reset(Bucket) ->
    cmdb_util:reset(Bucket, cmcloud:current_nodes()).

backup(_Db) ->  ok.
restore(_Db, _Name) ->  ok.
