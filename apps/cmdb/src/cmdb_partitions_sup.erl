-module(cmdb_partitions_sup).
-behaviour(supervisor).
-export([start_link/1, start_partition/1]).
-export([init/1]).

start_link(#{ name := Name }=Bucket) ->
    RegName = cmdb_config:partitions_sup(Name),
    supervisor:start_link({local, RegName}, ?MODULE, [Bucket]).

init([_]) ->
    
    Spec = cmkit:child_spec(cmdb_partition,
                            cmdb_partition,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start_partition(#{ name := Name}=Partition) ->
    RegName = cmdb_config:partitions_sup(Name),
    {ok, _} = supervisor:start_child(RegName, [Partition]).
