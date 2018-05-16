-module(cmtest_runner).
-behaviour(gen_statem).
-export([
         run/1,
         run/2,
         stop/0,
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).

run(#{ scenarios := Scenarios }=Test) ->
    run(Test, Scenarios).

run(Test, Scenarios) ->
    gen_statem:call(?MODULE, {scenarios, Test, Scenarios}).

stop() ->
    gen_statem:call(?MODULE, stop).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Log = cmkit:log_fun(true),
    Log({cmtest, node(), self()}),
    {ok, ready, #{ log => Log }}.

ready({call, From}, {scenarios, #{ name := Name}=Spec, [S|Rem]=Scenarios}, Data) ->
    Rep = {running, Name, length(Scenarios)},
    {ok, Pid } = cmtest_util:run(Spec, S),
    {keep_state, Data#{ stats => #{ 
                          pid => Pid,
                          started => cmkit:now(),
                          total => 0, 
                          pending => length(Scenarios), 
                          success => 0,
                          error => 0,
                          running => 1,
                          scenarios => [] 
                         }
                      }, [{reply, From, Rep}]};

ready({call, From}, {scenarios, #{ name := Name}, []}, Data) ->
    Rep = {no_scenarios, Name, 0},
    {keep_state, Data, [{reply, From, Rep}]};

ready({call, From}, stop, #{ stats := #{ pid := Pid }}=Data) ->
    cmtest_scenario_sup:kill(Pid),
    {keep_state, Data#{ stats => null }, [{reply, From, ok}]}.

terminate(Reason, _, #{ log := Log}) ->
    Log({cmtest, node(), self(), terminated, Reason}),
    ok.

report(Name, {Time, Res}) ->
    {Name, Time/1000, Res}.
