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
    {ok, ready, #{ log => Log, 
                   started => cmkit:now(),
                   failures => [] 
                 }}.

ready({call, From}, {scenarios, Test, Scenarios, #{ name := SettingsName,
                                                    spec := SettingsSpec }}, Data) ->
    
    Data2 = Data#{ report_to => From, 
                   settings => #{ name => SettingsName },
                   query => test,
                   test => Test,
                   total => 0,
                   success => 0,
                   fail => 0,
                   failures => [] },

    case cmencode:encode(SettingsSpec) of 
        {ok, EncodedSettings} -> 
            
            Data3 = Data2#{ settings => #{ name => SettingsName,
                                           value => EncodedSettings }},
            
            case start_scenario(Test, Scenarios, Data3) of 
                {ok, Data4} ->
                    {keep_state, Data4, ok(From)}; 
                {finished, Data4} ->
                    report(Data4),
                    {stop_and_reply, normal, ok(From)};
                Other ->
                    notify_error(Other, Data3),
                    {stop_and_reply, normal, ok(From)}
            end;
        
        {error, E} ->
            notify_error(E, Data),
            {stop_and_reply, normal, ok(From)}
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
            report(Data3),
            {stop, normal};
        {error, E} ->
            notify_error(E, Data2),
            {stop, normal}
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
            report(Data3),
            {stop, normal};
        {error, E} ->
            notify_error(E, Data2),
            {stop, normal}
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
          settings := #{ name := SettingsName }=Settings,
          started := T1,
          finished := T2,
          failures := Failures,
          total := Total, 
          success := Success,
          fail := Fail 
        }=Data) -> 

    S = severity(Fail, Success, Total),
    Millis = trunc((T2-T1)/1000),

    Q = query(Data),

    Stats = #{ total => Total,
               success => Success,
               fail => Fail },

    Report = #{ timestamp => cmkit:now(),
                query => Q,
                settings => SettingsName,
                status => ok,
                scenarios => Stats,
                millis => Millis,
                failures => Failures},

    cmkit:S({cmtest, Q, SettingsName, Failures, Stats, Millis}),
    save_report(Report),
    gen_statem:cast(From, finished),
    slack(Settings, #{ test => Q,
                       severity => S, 
                       stats => Stats }).


notify_error(_, #{ report_to := { From, _ },
                            settings := Settings  }=Data) ->

    Q = query(Data),
    gen_statem:cast(From, finished),
    slack(Settings, #{ test => Q,
                       severity => danger}).




severity(_, _, 0) -> success;
severity(0, _, _) -> success;
severity(_, 0, _) -> danger;
severity(_, _, _) -> warning.


query(#{ query := all }) -> all;
query(#{ query := test, test := #{ name := Name}}) -> Name.

save_report(#{ query := Query} = Report) -> 
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
          }}, #{ test := Q, 
                 severity :=S }=Msg) ->

    Env = cmkit:to_bin(Name),
    Test = cmkit:to_bin(Q),

    { Subject, Text } = case Msg of 
                            #{ stats := #{ 
                                 total := Total,
                                 fail := Fail,
                                 success := Success }
                             } ->
                                
                                TotalBin = cmkit:to_bin(Total),
                                SuccessBin = cmkit:to_bin(Success),
                                FailBin = cmkit:to_bin(Fail),
                                { <<"Test ", Test/binary, " finished on ", Env/binary>>,
                                  <<TotalBin/binary, " scenario(s), ", 
                                    SuccessBin/binary, " passed, ", 
                                    FailBin/binary, " failed">> };


                            _ -> 
                                
                                { <<"Test ", Test/binary, " failed to run on", Env/binary>>,
                                  <<"Check the server logs for more detail">> }
                                
                        end, 

    cmslack:S(#{ token => T,
                 channel => Ch,
                 subject => Subject,
                 text => Text });

slack(_, _) -> 
    ok.
