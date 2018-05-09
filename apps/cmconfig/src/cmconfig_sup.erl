-module(cmconfig_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Specs = lists:map(fun worker_spec/1, [cmconfig_cache,
                                          cmconfig_compiler, 
                                          cmconfig_watcher,
                                          cmconfig_loader]),
    {ok, { {one_for_one, 0, 1}, Specs} }.

worker_spec(Mod) ->
    cmkit:child_spec(Mod, Mod, [], worker).
