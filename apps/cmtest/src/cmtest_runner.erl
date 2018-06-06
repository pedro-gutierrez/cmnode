-module(cmtest_runner).
-behaviour(gen_statem).
-export([
         run/2,
         run/3,
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


run(#{ scenarios := Scenarios }=Test, Settings) ->
    run(Test, Scenarios, Settings).

run(Test, Scenarios, Settings) ->
    case cmtest_runner_sup:start_child() of 
        {ok, Pid} ->
            gen_statem:call(Pid, {scenarios, Test, Scenarios, Settings});
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

ready({call, From}, {scenarios, Test, Scenarios, #{ name := SettingsName,
                                                    spec := SettingsSpec }}, Data) ->
    case cmencode:encode(SettingsSpec) of 
        {ok, EncodedSettings} -> 
            case start_scenario(Test, Scenarios, Data#{ report_to => From,
                                                        settings => #{ name => SettingsName,
                                                                       value => EncodedSettings },
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
        
        {error, E} ->
            report(E, Data)
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

start_scenario(#{ name := Name }=Test, [S|Rem], #{ total := Total,
                                                   settings := #{ value := Settings } }=Data) ->
    case cmtest_util:start(Test, S, Settings, self() ) of 
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
          failures := Failures,
          settings := #{ name := SettingsName }=Settings
        }=Data) ->
    
    S = severity(Fail, Success, Total),
    Millis = trunc((T2-T1)/1000),
    
    Q = query(Data),
    Report = #{ timestamp => cmkit:now(),
                query => Q,
                settings => SettingsName,
                status => ok,
                scenarios => #{ total => Total,
                                success => Success,
                                fail => Fail },
                millis => Millis,
                failures => Failures},

    cmkit:S({cmtest, Report}),
    save(Report),
    gen_statem:cast(From, finished),
    slack(Settings, Q, S, Total, Success, Fail),
    {stop, normal}.

report(Error, #{ report_to := {From, _},
                 settings := Settings }=Data) ->
    
    Q = query(Data),
    Report = #{ query => Q,
                status => error,
                error => Error },
    
    cmkit:danger({cmtest, Report}),
    save(Report), 
    gen_statem:cast(From, finished),
    slack(Settings, Q, Error),
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


slack(#{ name := Name,
         value := #{ 
           slack := #{ 
             tests := #{ 
               enabled := true,
               token := T,
               channel := Ch
              }
            }
          }
       }, Q, S, Total, Success, Fail) ->

    Env = cmkit:to_bin(Name),
    Test = cmkit:to_bin(Q),
    TotalBin = cmkit:to_bin(Total),
    SuccessBin = cmkit:to_bin(Success),
    FailBin = cmkit:to_bin(Fail),
    Subject = <<"Test ", Test/binary, " did finish on ", Env/binary>>,
    Text = <<TotalBin/binary, " scenario(s), ", 
             SuccessBin/binary, " passed, ", 
             FailBin/binary, " failed">>,

    cmslack:S(#{ token => T,
                 channel => Ch,
                 subject => Subject,
                 text => Text });

slack(_, Q, _, _, _, _) ->
    cmkit:log({cmtest, Q, slack_notification, skipped}).


slack(#{ name := Name,
         value := #{ 
           slack := #{ 
             tests := #{ 
               enabled := true,
               token := T,
               channel := Ch
              }
            }
          }
       }, Q, _) ->
    
    Env = cmkit:to_bin(Name),
    Test = cmkit:to_bin(Q),
    Subject = <<"Test ", Test/binary, " failed to run on ", Env/binary>>,
    Text = <<"Visit https://tests.netc.io for more details">>,
    cmslack:danger(#{ token => T,
                 channel => Ch,
                 subject => Subject,
                 text => Text });

slack(_, Q, _) ->
    cmkit:log({cmtest, Q, slack_notification, skipped}).


