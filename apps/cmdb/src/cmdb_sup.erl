-module(cmdb_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Directory = cmkit:child_spec(cmdb_directory, 
                                 cmdb_directory, 
                                 [],
                                 permanent,
                                 worker),
    
    BucketsSup = cmkit:child_spec(cmdb_buckets_sup,
                                  cmdb_buckets_sup,
                                  [],
                                  permanent,
                                  supervisor),

    {ok, {{one_for_one, 0, 1}, [Directory, BucketsSup]}}.
