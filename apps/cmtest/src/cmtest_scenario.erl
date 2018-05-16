-module(cmtest_scenario).
-behaviour(gen_statem).
-export([
         start/2,
         run/1,
         start_link/2,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).

start(Test, Spec) ->
    cmtest_scenario_sup:start(Test, Spec).

run(Pid) ->
    gen_statem:call(Pid, run).

callback_mode() ->
    state_functions.

start_link(Test, Spec) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Test, Spec], []).

init([#{ name := Name }=Test, #{ title := Title }=Spec]) ->
    Log = cmkit:log_fun(true),
    Log({cmtest, started, Name, Title, self()}),
    {ok, ready, #{ log => Log, test => Test, scenario => Spec}}.

ready({call, From}, run, #{ log := Log, test := #{ name := Name }, scenario := #{ title := Title}}=Data) ->
    Log({cmtest, running, Name, Title, self()}),
    {keep_state, Data, [{reply, From, ok}]}.

terminate(Reason, _, #{ log := Log, test := #{ name := Name }, scenario := #{ title := Title }}) ->
    Log({cmtest, terminated, Name, Title, self(), Reason}),
    ok.
