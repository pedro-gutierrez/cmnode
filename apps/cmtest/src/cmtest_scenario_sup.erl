-module(cmtest_scenario_sup).
-behaviour(supervisor).
-export([start_link/0, start/6, kill/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Spec = cmkit:child_spec(cmtest_scenario,
                            cmtest_scenario,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start(Test, Scenario, Steps, Facts, Settings, Runner) ->
    supervisor:start_child(?MODULE, [Test, Scenario, Steps, Facts, Settings, Runner]).

kill(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
