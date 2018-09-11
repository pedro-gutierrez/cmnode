-module(cmdb_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(Buckets) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Buckets]).

init([Buckets]) ->
    
    Writers = [writer_spec(B) || #{ storage := disc }=B <- Buckets],
    Specs = Writers,
    {ok, { {one_for_one, 0, 1}, Specs} }.

writer_spec(#{ name := Name}=B) ->
    Writer = cmdb_config:writer(Name),
    cmkit:child_spec(Writer,
                     cmdb_writer,
                     [B#{ writer => Writer }],
                     permanent,
                     worker).

