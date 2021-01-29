-module(cmtest_runner_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/0, kill/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Spec = cmkit:child_spec(cmtest_runner,
                            cmtest_runner,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start_child() ->
    supervisor:start_child(?MODULE, []).

kill(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
