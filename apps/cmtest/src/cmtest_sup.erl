-module(cmtest_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    Runner = cmkit:child_spec(cmtest_runner,
                            cmtest_runner,
                            [],
                            permanent,
                            worker),

    ScenarioSup = cmkit:child_spec(cmtest_scenario_sup,
                                   cmtest_scenario_sup,
                                   [],
                                   permanent,
                                   supervisor),

    
    {ok, { {one_for_one, 0, 1}, [
                                 Runner, 
                                 ScenarioSup
                                ]}}.

