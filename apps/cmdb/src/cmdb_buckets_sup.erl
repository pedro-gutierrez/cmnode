-module(cmdb_buckets_sup).
-behaviour(supervisor).
-export([start_link/0, start_bucket/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    Spec = cmkit:child_spec(cmdb_bucket_sup,
                            cmdb_bucket_sup,
                            [],
                            temporary,
                            supervisor),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start_bucket(#{ name := Name }=Bucket) ->
    case supervisor:start_child(?MODULE, [Bucket]) of 
        {ok, _} ->
            cmdb_util:load_partitions(Name);
        Other -> 
            cmkit:danger({cmdb, start, Name, Other})
    end.

