-module(cmdb_bucket).
-behaviour(gen_server).
-export([start_link/1]).
-export([put/3, 
         put_new/3,
         register_partition/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

put(Bucket, Pairs, Opts) ->
    Partition = cmdb_util:current_partition(Bucket),
    gen_server:call(Partition, {put, Pairs, Opts}).

put_new(Bucket, Pairs, Opts) ->
    Partition = cmdb_util:current_partition(Bucket),
    gen_server:call(Partition, {put_new, Pairs, Opts}).

register_partition(P) ->
    call(P).

call(#{ name := Name }=P) -> 
    RegName = cmdb_config:bucket(Name),
    gen_server:cast(RegName, {register, P}).


start_link(#{ name := Name}=Bucket) ->
    RegName = cmdb_config:bucket(Name),
    gen_server:start_link({local, RegName},?MODULE, [Bucket], []).

init([Bucket]) ->
    {ok, Bucket#{ current => none,
                  partitions => []}}.

handle_call(load, _, Bucket) ->
    Partitions = cmdb_util:find_partitions(Bucket),
    lists:foreach(fun(Location) ->
                     cmdb_partitions_sup:start_partition(Bucket#{ partition => Location})     
                  end, Partitions),
    {reply, ok, Bucket}.


handle_cast({register, #{ name := Bucket, 
                          tid := Tid }}, #{ name := Bucket,
                                            current := Current,
                                            partitions := Partitions }=State) ->
    Current2 = case Current of 
                   none -> Tid;
                   Tid0 -> 
                        #{ memory := M} = cmdb_util:partition_info(Bucket, Tid0),
                        #{ memory := M2} = cmdb_util:partition_info(Bucket, Tid),
                        case M2 > M of 
                            true -> Tid;
                            false -> Current
                        end
               end,

    State2 = State#{ current => Current2, 
                     partitions => [Tid|Partitions]},
    ok = cmdb_util:cache_partitions(State2),
    {noreply, State2};

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #{ name := Name}) ->
    cmkit:log({cmdb, bucket, Name, node(), Reason}),
    ok.
