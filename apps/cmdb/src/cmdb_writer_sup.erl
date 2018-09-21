-module(cmdb_writer_sup).
-behaviour(supervisor).
-export([start_link/0, start/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    Writer = cmkit:child_spec(cmdb_writer,
                              cmdb_writer,
                              [],
                              permanent,
                              worker),
    
    {ok, { {simple_one_for_one, 0, 1}, [Writer]}}.

start(Bucket) ->
    supervisor:start_child(?MODULE, [Bucket]).
