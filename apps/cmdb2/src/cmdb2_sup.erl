-module(cmdb2_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    

    Writers = [writer_spec(B) || #{ storage := disc }=B <- cmconfig:buckets()],
    Specs = Writers,
    
    {ok, { {one_for_one, 0, 1}, Specs} }.

writer_spec(#{ name := Name}=B) ->
    Writer = cmdb_config:writer(Name),
    cmkit:child_spec(Writer,
                     cmdb2_writer,
                     [B#{ writer => Writer }],
                     permanent,
                     worker).

