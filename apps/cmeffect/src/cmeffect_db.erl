-module(cmeffect_db).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db.

effect_apply(#{ context := C,
                put := ToPut,
                delete := ToDelete }, Id) ->

    R = reply_from(put_del(by_bucket(ToPut, ToDelete))),
    cmcore:update(Id, R#{ context => C });



effect_apply(#{ context := C,
                bucket := B,
                put := ToPut,
                delete := ToRemove }, Id)  ->

    R = reply_from(cmdb:put_del(B, values(ToPut), keys(ToRemove))),
    cmcore:update(Id, R#{ context => C }).

key(#{ subject := S,
       predicate := P,
       object := O }) -> {S, P, O};

key(#{ subject := S,
       predicate := P}) -> {S, P};

key(#{ subject := S }) -> {S}.


value(#{ subject:= S,
         predicate := P,
         object := O,
         value := V}) -> {S, P, O, V}.

keys(K) when is_list(K) -> lists:map(fun key/1, K);
keys(K) when is_map(K) -> [key(K)].


values(V) when is_list(V) -> lists:map(fun value/1, V);
values(V) when is_map(V) -> [value(V)].

reply_from(ok) -> #{ status => ok };
reply_from({ok, Data}) -> #{ status => ok,
                              data => Data };
reply_from({error, E}) -> #{ status => error,
                            error => E };
reply_from(Other) -> #{ status => unknown,
                    data => Other }.


by_bucket(ToAdd, ToDelete) ->
    to_put(ToAdd, to_delete(ToDelete, #{})).

to_delete([], ByBucket) -> ByBucket;
to_delete([K|Rem], ByBucket) ->
    to_delete(Rem, to_delete(K, ByBucket));

to_delete(#{ bucket := B}=K, ByBucket) ->
    #{ delete := Keys } = BucketIndex = bucket_index(B, ByBucket),
    ByBucket#{ B => BucketIndex#{ delete => [key(K)|Keys]}}.

to_put([], ByBucket) -> ByBucket;
to_put([K|Rem], ByBucket) ->
    to_put(Rem, to_put(K, ByBucket));

to_put(#{ bucket := B}=V, ByBucket) ->
    #{ put := Values} = BucketIndex = bucket_index(B, ByBucket),
    ByBucket#{ B => BucketIndex#{ put=> [value(V)|Values]}}.

bucket_index(B, ByBucket) ->
    maps:get(B, ByBucket, #{ delete => [],
                             put => []}).

put_del(Buckets) ->
    put_del(maps:keys(Buckets), Buckets).

put_del([], _) -> ok;
put_del([B|Rem], Buckets) ->
    #{ put := ToPut,
       delete := ToDelete } = maps:get(B, Buckets),
    case cmdb:put_del(B, ToPut, ToDelete) of 
        ok ->
            put_del(Rem, Buckets);
        Other ->
            Other
    end.
