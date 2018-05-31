-module(cmtest_runner).
-behaviour(gen_statem).
-export([
         run/1,
         run/2,
         progress/4,
         success/4,
         fail/4,
         stop/0,
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).


run(#{ scenarios := Scenarios }=Test) ->
    run(Test, Scenarios);

run(All) ->
    case cmtest_runner_sup:start_child() of 
        {ok, Pid} ->
            gen_statem:call(Pid, {tests, All});
        Other -> 
            Other
    end.

run(Test, Scenarios) ->
    case cmtest_runner_sup:start_child() of 
        {ok, Pid} ->
            gen_statem:call(Pid, {scenarios, Test, Scenarios});
        Other -> 
            Other
    end.

progress(T, S, Info, Pid) ->
    gen_statem:cast(Pid, {progress, T, S, Info}).

success(T, S, Info, Pid) ->
    gen_statem:cast(Pid, {success, T, S, Info}).

fail(T, S, Info, Pid) ->
    gen_statem:cast(Pid, {fail, T, S, Info}).

ok(From) -> reply(From, ok).

reply(From, Msg) -> [{reply, From, Msg}].

stop() ->
    gen_statem:call(?MODULE, stop).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init([]) ->
    Log = cmkit:log_fun(true),
    Log({cmtest, runner, self()}),
    {ok, ready, #{ log => Log, 
                   started => cmkit:now(),
                   failures => [] 
                 }}.


ready({call, From}, {tests, Tests}, Data) ->
    case start_test(Tests, Data#{ report_to => From,
                                  query => all,
                                  tests => Tests,
                                  total => 0,
                                  success => 0,
                                  fail => 0, 
                                  failures => [] } ) of 
        {ok, Data2} ->
            {keep_state, Data2, ok(From)}; 
        {finished, Data2} ->
            report(Data2);
        Other ->
            report(Other, Data)
    end;
    
ready({call, From}, {scenarios, Test, Scenarios}, Data) ->
    case start_scenario(Test, Scenarios, Data#{ report_to => From,
                                                query => test,
                                                test => Test,
                                                total => 0,
                                                success => 0,
                                                fail => 0, 
                                                failures => [] } ) of 
        {ok, Data2} ->
            {keep_state, Data2, ok(From)}; 
        {finished, Data2} ->
            report(Data2);
        Other ->
            report(Other, Data)
    end;

ready({call, From}, stop, #{ pid := Pid }=Data) ->
    cmtest_scenario:stop(Pid),
    {keep_state, Data#{ stats => null }, [{reply, From, ok}]};

ready(cast, {progress, _, _, _}, #{ pid := Pid }=Data) ->
    cmtest_scenario:next(Pid),
    {keep_state, Data};

ready(cast, {success, _, _, _}, #{ 
                                      pid := Pid,
                                      test := Test,
                                      scenarios:= Rem,
                                      success := Success
                                    }=Data) ->
    cmtest_scenario:stop(Pid),
    Data2 = Data#{ success => Success + 1 },
    case start_scenario(Test, Rem, Data2) of 
        {ok, Data3 } ->
            {keep_state, Data3};
        {finished, Data3} ->
            report(Data3);
        {error, E} ->
            report(E, Data2)
    end;

ready(cast, {fail, _, Title, Info
            }, #{  
                  pid := Pid,
                  test := #{ name := Name }=Test,
                  scenarios := Rem,
                  fail := Fail,
                  failures := Failures
                }=Data) ->
    
    Data2 = Data#{ fail => Fail + 1,
                   failures => [#{ test => Name,
                                   scenario => Title,
                                   failure => Info }
                                |Failures]
                 },
    cmtest_scenario:stop(Pid),
    case start_scenario(Test, Rem, Data2) of 
        {ok, Data3} ->
            {keep_state, Data3};
        {finished, Data3} ->
            report(Data3);
        {error, E} ->
            report(E, Data2)
    end.
    
terminate(_, _, _) ->
    ok.

start_test([], Data) ->
    {finished, Data#{ finished => cmkit:now() }};

start_test([#{ scenarios := Scenarios}=T|Rem], Data) ->
    start_scenario(T, Scenarios, Data#{ test => T,
                                        tests => Rem}).

start_scenario(_, [], #{ tests := [] } = Data) ->
    {finished, Data#{ finished => cmkit:now() }};

start_scenario(_, [], #{ tests := Tests }=Data) ->
    start_test(Tests, Data);

start_scenario(#{ name := Name }=Test, [S|Rem], #{ total := Total }=Data) ->
    case cmtest_util:start(Test, S, self() ) of 
        {ok, Pid} ->
            Data2 = Data#{ 
                      pid => Pid,
                      scenarios => Rem,
                      total => Total + 1
                     },
            cmtest_scenario:next(Pid),
            {ok, Data2};
        {error, {Title, {error, Reason}}} ->
            {error, #{ test => Name, 
                       scenario => Title, 
                       reason => Reason}}
    end;

start_scenario(_, _, Data) ->
    {finished, Data#{ finished => cmkit:now() }}.

report(#{ report_to := {From, _},
          started := T1, 
          finished := T2,
          total := Total,
          success := Success,
          fail := Fail,
          failures := Failures 
        }=Data) ->

    S = severity(Fail, Success, Total),
    Millis = trunc((T2-T1)/1000),

    Report = #{ timestamp => cmkit:now(),
                query => query(Data),
                status => ok,
                scenarios => #{ total => Total,
                                success => Success,
                                fail => Fail },
                millis => Millis,
                failures => Failures},

    cmkit:S({cmtest, Report}),
    gen_statem:cast(From, finished),
    save(Report),
    {stop, normal}.

report(Error, #{ report_to := {From, _}}=Data) ->
    
    Report = #{ query => query(Data),
                status => error,
                error => Error },
    
    cmkit:danger({cmtest, Report}),
    gen_statem:cast(From, finished),
    save(Report), 
    {stop, normal}.


severity(_, _, 0) -> success;
severity(0, _, _) -> success;
severity(_, 0, _) -> danger;
severity(_, _, _) -> warning.


query(#{ query := all }) -> all;
query(#{ query := test, test := #{ name := Name}}) -> Name.

save(#{ query := Query} = Report) -> 
    Id = cmkit:uuid(),
    Now = calendar:local_time(), 
    Pairs = [{{report, Id},Report#{ id => Id } }, 
             {{report, Query}, Id},
             {{report, cmcalendar:to_bin(Now, year)}, Id},
             {{report, cmcalendar:to_bin(Now, month)}, Id},
             {{report, cmcalendar:to_bin(Now, date)}, Id}
            ],

    cmdb:put(tests, Pairs).
