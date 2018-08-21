-module(cmtest_runner).
-behaviour(gen_statem).
-export([
         run/2,
         run/3,
         progress/5,
         success/5,
         fail/5,
         stop/1,
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

progress(T, S, Info, Elapsed, Pid) ->
    gen_statem:cast(Pid, {progress, T, S, Info, Elapsed}).

success(T, S, Info, Elapsed, Pid) ->
    gen_statem:cast(Pid, {success, T, S, Info, Elapsed}).

fail(T, S, Info, Elapsed, Pid) ->
    gen_statem:cast(Pid, {fail, T, S, Info, Elapsed}).

ok(From) -> reply(From, ok).

reply(From, Msg) -> [{reply, From, Msg}].

stop(Id) ->
    case erlang:whereis(?MODULE) of 
        undefined ->
            ok;
        _ -> 
            gen_statem:call(?MODULE, {stop, Id})
    end.

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

ready({call, From}, {scenarios, #{ name := TestName }=Test, Scenarios, #{ name := SettingsName,
                                                    spec := SettingsSpec }}, #{ log := Log }=Data) ->
    
    Data2 = Data#{ report_to => From, 
                   settings => #{ name => SettingsName },
                   query => test,
                   test => Test,
                   total => 0,
                   success => 0,
                   fail => 0,
                   result => [] },

    Log({cmtest, TestName, scenarios, length(Scenarios) }),
    case cmencode:encode(SettingsSpec) of 
        {ok, EncodedSettings} -> 
            
            EncodedSettings2 = case maps:get(opts, Test, undef) of 
                                   undef -> EncodedSettings;
                                   Opts -> 
                                       maps:merge(EncodedSettings, Opts)
                               end,
            Data3 = Data2#{ settings => #{ name => SettingsName,
                                           value => EncodedSettings2 }},
            
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
            notify_error(E, Data2),
            {stop_and_reply, normal, ok(From)}
    end;


ready({call, From}, {stop, _}, #{ pid := Pid }=Data) ->
    cmtest_scenario:stop(Pid),
    {keep_state, Data#{ stats => null }, [{reply, From, ok}]};

ready(cast, {progress, _, Scenario, StepsRem, Elapsed}, #{ pid := Pid }=Data) ->
    notify_progress(Scenario, StepsRem, Elapsed, Data),
    cmtest_scenario:next(Pid),
    {keep_state, Data};

ready(cast, {success, _, Title, _, Elapsed }, #{ 
                                      test := #{ name := Name }=Test,
                                      scenarios:= Rem,
                                      success := Success,
                                      result := Result
                                    }=Data) ->
    
    Data2 = stop_scenario(Data),
    Data3 = Data2#{ success => Success + 1, 
                   result => [#{ test => Name,
                                 scenario => Title, 
                                 status => success,
                                 elapsed => Elapsed }|Result] },
    case start_scenario(Test, Rem, Data3) of 
        {ok, Data4} ->
            {keep_state, Data4};
        {finished, Data4} ->
            report(Data4),
            {stop, normal};
        {error, E} ->
            notify_error(E, Data3),
            {stop, normal}
    end;

ready(cast, {fail, _, Title, Info, Elapsed }, #{ test := Test, scenarios := Rem }=Data) ->
    Data2 = data_with_failure(Test, Title, Info, Elapsed, Data),
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

start_scenario(Test, [#{title := Title}=S|Rem], #{ total := Total, settings := #{ value := Settings } }=Data) ->
    case cmtest_util:start(Test, S, Settings, self() ) of 
        {ok, Pid} ->
            Data2 = Data#{ 
                      pid => Pid,
                      scenarios => Rem,
                      total => Total + 1
                     },
            cmtest_scenario:next(Pid),
            {ok, Data2};
        {error, E} ->
            Data2 = data_with_failure(Test, Title, #{ reason => E,
                                                      step => #{},
                                                      world => #{} }, 0, Data#{ scenarios => Rem }),
            start_scenario(Test, Rem, Data2)
    end;

start_scenario(_, _, Data) ->
    {finished, Data#{ finished => cmkit:now() }}.

data_with_failure(Test, Title, #{ step := _, world := _, reason := _ } = Error, Elapsed, #{ log := Log,
                                                                                            test := #{ name := Name }=Test,
                                                                                            scenarios := Rem,
                                                                                            fail := Fail,
                                                                                            result := Result } = Data) -> 
    
    Log({cmtest, Name, failed, Title, remaining, length(Rem)}),
    Data2 = stop_scenario(Data),
    Data3 = Data2#{ fail => Fail + 1, result => [#{ test => Name, 
                                                    scenario => Title,
                                                    status => fail,
                                                    failure => Error,
                                                    elapsed => Elapsed }|Result] },
    Data3.

stop_scenario(#{ pid := Pid }=Data) ->
    cmkit:log({cmtest, stopping_senario, Pid}),
    ok = cmtest_scenario:stop(Pid),
    maps:without([pid], Data);

stop_scenario(Data) -> Data. 

report(#{ settings := #{ name := SettingsName }=Settings,
          result := R,
          total := Total, 
          success := Success,
          fail := Fail 
        }=Data) -> 

    Result = lists:reverse(R),
    S = severity(Fail, Success, Total),

    Q = query(Data),

    Secs = lists:foldl(fun(#{ elapsed := Elapsed }, Acc) ->
                            Acc + Elapsed
                         end, 0, Result) div 1000,
    
    Stats = #{ total => Total,
               success => Success,
               fail => Fail,
               seconds => Secs },
        
    Failures = lists:filter(fun(#{ status := fail }) -> true;
                               (_) -> false 
                            end, Result),
   


    Report = #{ timestamp => cmkit:now(),
                query => Q,
                settings => SettingsName,
                status => ok,
                scenarios => Stats,
                seconds => Secs,
                result => Result},

    cmkit:S({cmtest, Q, SettingsName, Failures, Stats, Secs}),
    save_report(Report),
    finish(Data),
    
    Summary = #{ test => Q,
                 severity => S,
                 stats => Stats },
    webhook(Data, Summary),
    slack(Settings, Summary).


finish(#{ report_to := {From, _},
          test := #{ name := Test, 
                     id := Id } }) ->

    gen_statem:cast(From, {finished, Id}),
    cmkit:log({cmtest, Test, finished});

finish(#{ test := #{ name := Test }}) ->
    
    cmkit:log({cmtest, Test, finished}).

notify_error(Reason, #{settings := Settings}=Data) ->

    cmkit:danger({cmtest, error, Reason}),
    Q = query(Data),
    finish(Data),
    slack(Settings, #{ test => Q,
                       severity => danger}).

notify_progress(Scenario, _, _, #{ report_to := {From, _},
                                settings := #{ name := Settings},
                                test := #{ id := Id,
                                           name := Name }}) ->

    gen_statem:cast(From, {info, Id, #{ test => Name,
                                        settings => Settings,
                                        info => Scenario }});

notify_progress(_, _, _, _) -> 
    ok.

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
    
    cmkit:log({cmtest, report, Id}),
    cmdb:put(tests, Pairs).

webhook(#{ settings := #{ name := SettingsName,
                          value := Settings }}=Data, Summary) ->
   
    Q = query(Data),
    case Settings of 
        #{ webhooks := #{ default := Url } } -> 
            case maps:get(webhook_opts, Settings, undef) of 
                undef -> 
                    cmkit:log({cmtest, Q, SettingsName, webhook, skipped, undef});
                #{ enabled := false } ->
                    cmkit:log({cmtest, Q, SettingsName, webhook, skipped, disabled});
                #{ enabled := true }  -> 
                    
                    Headers = #{ 'content-type' => <<"application/json">>,
                                 'x-cm-event' => <<"test-summary">> },
                    
                    Summary2 = Summary#{ test => Q,
                                         settings => SettingsName },
                    
                    cmkit:log({cmtest, Q, webhook, enabled, Url, Headers, Summary2}),
                    cmhttp:post(Url, Headers, Summary2)
            end;
        _ -> 
            cmkit:log({cmtest, Q, SettingsName, webhook, skipped, no_default_url})
    end.

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
                                 success := Success,
                                 seconds := Secs }
                             } ->
                                
                                TotalBin = cmkit:to_bin(Total),
                                SuccessBin = cmkit:to_bin(Success),
                                FailBin = cmkit:to_bin(Fail),
                                SecsBin = cmkit:to_bin(Secs),

                                { <<"Test ", Test/binary, " finished on ", Env/binary>>,
                                  <<TotalBin/binary, " scenario(s), ", 
                                    SuccessBin/binary, " passed, ", 
                                    FailBin/binary, " failed, in ", SecsBin/binary , " seconds">> };

                            _ -> 
                                
                                { <<"Test ", Test/binary, " failed to run on ", Env/binary>>,
                                  <<"Check the server logs for more detail">> }
                                
                        end, 

    cmslack:S(#{ token => T,
                 channel => Ch,
                 subject => Subject,
                 text => Text });

slack(_, _) -> 
    ok.
