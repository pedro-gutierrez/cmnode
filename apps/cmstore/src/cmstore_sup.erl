-module(cmstore_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Stores) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Stores]).

init([Stores]) ->
    Monitor = cmkit:child_spec(cmstore_monitor,
                               cmstore_monitor,
                               [Stores],
                               permanent,
                               worker),
    {ok, {{one_for_one, 0, 1}, [Monitor]}}.
