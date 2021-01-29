-module(cmconfig_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->


    Watcher = cmkit:child_spec(cmconfig_watcher,
                               cmconfig_watcher,
                               [],
                               permanent,
                               worker),

    CompilerSup = cmkit:child_spec(cmconfig_compiler_sup,
                                   cmconfig_compiler_sup,
                                   [],
                                   permanent,
                                   supervisor),

    {ok, { {one_for_one, 0, 1}, [Watcher, CompilerSup]} }.
