-module(cmtest_util).
-export([run/2,
         find_scenarios/2]).

run(Test, #{ title := Title}=Scenario) ->
    case cmtest_scenario:start(Test, Scenario) of 
        {ok, Pid} ->
            cmtest_scenario:run(Pid), 
            {ok, Pid};
        Other -> 
            {error, {Title, Other}}
    end.

find_scenarios(Token, Scenarios) ->
    {ok, lists:filter(fun(#{ tags := Tags}) ->
                              lists:member(Token, Tags)
                      end, Scenarios)}.
