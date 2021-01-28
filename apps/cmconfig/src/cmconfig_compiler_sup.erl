-module(cmconfig_compiler_sup).
-behaviour(supervisor).
-export([start_link/0, start/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Compiler = cmkit:child_spec(cmconfig_compiler,
                                cmconfig_compiler,
                                [],
                                temporary,
                                worker),

    {ok, { {simple_one_for_one, 0, 1}, [Compiler]}}.

start() ->
    supervisor:start_child(?MODULE, []).
