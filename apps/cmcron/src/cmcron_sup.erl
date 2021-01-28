-module(cmcron_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    Server = cmkit:child_spec(cmcron_server,
                              cmcron_server,
                              [],
                              permanent,
                              worker),

    {ok, { {one_for_one, 0, 1}, [ Server ]} }.
