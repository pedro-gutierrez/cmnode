-module(cmdb_bucket_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(#{ name := Name }=Bucket) ->
    RegName = cmdb_config:sup(Name),
    supervisor:start_link({local, RegName}, ?MODULE, [Bucket]).

init([Bucket]) ->

    Specs = [ cmkit:child_spec(Mod, Mod, [Bucket], permanent, Type) 
              || {Mod, Type} <- [{ cmdb_partitions_sup, supervisor },
                                 { cmdb_replicator, worker},
                                 { cmdb_bucket, worker}]],
    

    {ok, {{one_for_one, 0, 1}, Specs }}.
