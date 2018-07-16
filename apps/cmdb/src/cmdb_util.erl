-module(cmdb_util).
-export([start_buckets/0,
         bucket_info/1,
         open_partition/1,
         find_partitions/1,
         load_partitions/1,
         cache_partitions/1,
         partitions/1,
         current_partition/1,
         partition_name/1,
         reload_config/0,
         reset/2,
         reset/1,
         reset_partition/2,
         find/2,
         find/3,
         get/2,
         get/3,
         put/3,
         put_new/3
        ]).

start_buckets() -> 
    [ start_bucket(B) || B <- cmconfig:buckets()].

start_bucket(#{ name := Name}=B) -> 
    Res = cmdb_buckets_sup:start_bucket(B),
    Sev = case Res of 
              ok -> log;
              _ -> danger
          end,
    cmkit:Sev({cmdb, Name, bucket, Res}).

current_partition(Bucket) ->
    M = cmdb_config:partitions_resolver(Bucket),
    M:current().

find_partitions(#{ storage := memory }) -> 
    [cmkit:to_atom(cmkit:uuid())];

find_partitions(#{ name := Bucket,
                   storage := disc }) ->
    Data =  cmkit:data(),
    Dir =  filename:join([Data, Bucket]),
    cmkit:mkdirp(Dir),
    case cmkit:files(Dir, "db") of 
        [] -> [filename:join([Dir, cmkit:to_list(cmkit:uuid()) ++ ".db"])];
        Files -> Files
    end;

find_partitions(#{ name := Bucket,
                   storage := Other }) ->
    cmkit:warning({cmdb, Bucket, storage, Other, not_supported}),
    [].

partition_name(N) when is_atom(N) -> N;
partition_name(Filename) when is_list(Filename) -> 
    cmkit:to_atom(filename:basename(Filename, ".db")).



open_partition(#{ partition := Partition,
                  storage := memory }) ->
    case ets:new(Partition, [protected, bag, named_table]) of
        {error, E} -> {error, E};
        Tid -> {ok, Tid}
    end;

open_partition(#{ partition := Filename,
                  storage := disc }) ->
    Partition = partition_name(Filename),
    dets:open_file(Partition, [{access, read_write},
                               {type, bag},
                               {file, Filename}]).
        
load_partitions(Name) -> 
    Bucket = cmdb_config:bucket(Name),
    ok = gen_server:call(Bucket, load),
    ok.


bucket_info(Bucket) ->
    lists:foldl(fun(Tid, Out) ->
                        Out#{ Tid => partition_info(Bucket, Tid) }
                end, #{}, partitions(Bucket)).

partition_info(Bucket, Tid) -> 
    Storage = cmdb_config:backend(Bucket),
    storage_info(Storage, Tid).

storage_info(ets, Tid) ->
    #{ values => ets:info(Tid, size),
       memory => ets:info(Tid, memory) * erlang:system_info(wordsize)};

storage_info(dets, Tid) ->
    #{ values => dets:info(Tid, no_objects),
       memory => dets:info(Tid, memory)}.

reload_config() ->
    cmtemplate:reload(),
    Buckets = cmconfig:buckets(),
    {ok, Bin} = cmtemplate:render(cmdb_config, #{ buckets => Buckets }),
    case cmcode:load_from_binary(<<Bin/binary, "\n">>) of 
        {module, cmdb_config} -> ok;
        Other -> 
            Other
    end.

cache_partitions(B) ->
    {ok, Bin} = cmtemplate:render(cmdb_partitions, B),
    case cmcode:load_from_binary(<<Bin/binary, "\n">>) of 
        {module, _M} -> 
            ok;
        Other -> 
            Other
    end.



reset(Bucket, Nodes) -> 
    { R1, _} = rpc:multicall(Nodes, cmdb_util, reset, [Bucket]),
    case aggregate(#{ bucket => Bucket,
                 empty => ok }, R1) of 
        {ok, V} ->  V;
        Other -> 
            {error, Other}
    end.

reset(Bucket) ->
    Partitions = partitions(Bucket),
    reset_partitions(Bucket, Partitions).

reset_partitions(_, []) -> ok;
reset_partitions(Bucket, [P|Rem]) -> 
    case cmdb_partition:reset(P) of 
        ok -> 
            reset_partitions(Bucket, Rem);
        Other ->
            cmkit:warning({cmdb, reset, Bucket, Other}),
            reset_partitions(Bucket, Rem)
    end.

reset_partition(Bucket, Tid) ->
    Storage = cmdb_config:backend(Bucket),
    case Storage:delete_all_objects(Tid) of 
        true -> ok;
        Other -> 
            Other
    end.

partitions(Bucket) ->
    M = cmdb_config:partitions_resolver(Bucket),
    M:all().

get(Bucket, K, Nodes) -> 
    { R1, _} = rpc:multicall(Nodes, cmdb_util, get, [Bucket, K]),
    aggregate(#{ bucket => Bucket,
                 empty => not_found,
                 key => K}, R1).
get(Bucket, K) -> 
    Storage = cmdb_config:backend(Bucket),
    Results = lists:map(fun(Tid) -> Storage:lookup(Tid, K) end, partitions(Bucket)), 
    aggregate(#{ bucket => Bucket,
                 empty => not_found,
                 key => K}, Results).

find(Bucket, Type, Nodes) -> 
    { R1, _} = rpc:multicall(Nodes, cmdb_util, find, [Bucket, Type]),
    aggregate(#{ bucket => Bucket,
                 empty => [],
                 type => Type}, R1).

find(Bucket, Type) ->
    Storage = cmdb_config:backend(Bucket),
    Partitions = partitions(Bucket),
    Results = lists:map(fun(Tid) -> 
                                Storage:match(Tid, {{Type, '_'}, '$1'}) 
                        end, Partitions), 
    aggregate(#{ bucket => Bucket,
                 empty => [],
                 type => Type}, Results).
    
aggregate(Context, Results) ->
    aggregate(Context, Results, []).

aggregate(#{ empty := not_found }, [], []) -> not_found;
aggregate(#{ empty := V}, [], []) -> {ok, V};
aggregate(_, [], Out) -> {ok, lists:flatten(Out)};
aggregate(Ctx, [not_found|Rem], Out) ->
    aggregate(Ctx, Rem, Out);
aggregate(Ctx, [[]|Rem], Out) -> 
    aggregate(Ctx, Rem, Out);

aggregate(Ctx, [ok|Rem], Out) -> 
    aggregate(Ctx, Rem, Out);
aggregate(Ctx, [{ok, Items}|Rem], Out) when is_list(Items) -> 
    aggregate(Ctx, Rem, [lists:map(fun({_, V}) -> V;
                                      (V) -> V 
                                   end, Items)|Out]);
aggregate(Ctx, [Items|Rem], Out) when is_list(Items) -> 
    aggregate(Ctx, Rem, [lists:map(fun({_, V}) -> V;
                                      (V) -> V 
                                   end, Items)|Out]);
aggregate(Ctx, [{error, E}|Rem], Out) -> 
    cmkit:warning({cmdb, aggregate, Ctx, error, E}),
    aggregate(Ctx, Rem, Out).

put(Bucket, Tid, Pairs) when is_list(Pairs) ->
    Storage = cmdb_config:backend(Bucket),
    case Storage:insert(Tid, Pairs) of 
        true -> ok;
        Other -> 
            Other
    end.

put_new(Bucket, Tid, Pairs) when is_list(Pairs) ->
    Storage = cmdb_config:backend(Bucket),
    case Storage:insert_new(Tid, Pairs) of 
        true -> ok;
        Other -> 
            Other
    end.
