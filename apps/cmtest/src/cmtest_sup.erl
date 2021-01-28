-module(cmtest_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    RunnerSup = cmkit:child_spec(cmtest_runner_sup,
                                 cmtest_runner_sup,
                                 [],
                                 permanent,
                                 supervisor),

    ScenarioSup = cmkit:child_spec(cmtest_scenario_sup,
                                   cmtest_scenario_sup,
                                   [],
                                   permanent,
                                   supervisor),

    WsSup = cmkit:child_spec(cmtest_ws_sup,
                             cmtest_ws_sup,
                             [],
                             permanent,
                             supervisor),

    {ok, { {one_for_one, 0, 1}, [
                                 RunnerSup, 
                                 ScenarioSup,
                                 WsSup
                                ]}}.

