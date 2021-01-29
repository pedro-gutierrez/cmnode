-module(cmtest_runner).
-behaviour(gen_statem).
-export([
         run/2,
         run/3,
         progress/6,
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

progress(T, S, Info, Rem, Elapsed, Pid) ->
    gen_statem:cast(Pid, {progress, T, S, Info, Rem, Elapsed}).

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
                                                                          spec := SettingsSpec }}, Data) ->

    cmkit:log({cmtest, TestName, SettingsName, length(Scenarios)}),
    Data2 = Data#{ report_to => From, 
                   settings => #{ name => SettingsName },
                   query => test,
                   test => Test,
                   total => length(Scenarios),
                   success => 0,
                   fail => 0,
                   result => [],
                   current => [] },

    case encoded_settings(SettingsSpec, Test) of 
        {ok, Settings} ->
            case encoded_facts(Test, Settings) of 
                {ok, Facts} ->

                    Data3 = Data2#{ 
                                    facts => Facts,
                                    settings => #{ name => SettingsName,
                                                   value => Settings }},

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
                Other ->
                    notify_error(Other, Data2),
                    {stop_and_reply, normal, ok(From)}
            end;
        Other ->
            notify_error(Other, Data2),
            {stop_and_reply, normal, ok(From)}
    end;


ready({call, From}, {stop, _}, #{ pid := Pid }=Data) ->
    cmtest_scenario:stop(Pid),
    {keep_state, Data#{ stats => null }, [{reply, From, ok}]};

ready(cast, {progress, _, Scenario, Step, StepsRem, Elapsed}, #{ pid := Pid,
                                                                 current := C }=Data) ->

    Data2 = Data#{ current => [Step#{ status => success, failure => #{} }|C]},
    notify_progress(Scenario, StepsRem, Elapsed, Data2),
    cmtest_scenario:next(Pid),
    {keep_state, Data2};

ready(cast, {success, TestTitle, Scenario, _, Elapsed }, #{ test := Test, 
                                                            scenarios:= Rem,
                                                            current := C,
                                                            success := Success,
                                                            result := R }=Data) ->

    Data2 = stop_scenario(Data),
    ScenarioResult = #{ test => TestTitle,
                        scenario => Scenario,
                        status => success,
                        elapsed => Elapsed,
                        failure => #{},
                        steps => lists:reverse(C)},
    Data3 = Data2#{ current => [],
                    result => [ScenarioResult|R],
                    success => Success + 1 },

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

ready(cast, {fail, TestTitle, Scenario, #{ failure := F} = Step, Elapsed }, #{ test := Test, 
                                                                               scenarios := Rem,
                                                                               current := C,
                                                                               result := R,
                                                                               fail := Fail }=Data) ->


    Data2 = stop_scenario(Data),
    ScenarioResult = #{ test => TestTitle,
                        scenario => Scenario,
                        status => fail,
                        elapsed => Elapsed,
                        failure => F,
                        steps => lists:reverse([Step#{ status => fail }|C])},
    Data3 = Data2#{ current => [],
                    result => [ScenarioResult|R],
                    fail => Fail + 1 },

    case start_scenario(Test, Rem, Data3) of 
        {ok, Data4} ->
            {keep_state, Data4};
        {finished, Data4} ->
            report(Data4),
            {stop, normal};
        {error, E} ->
            notify_error(E, Data3),
            {stop, normal}
    end.

terminate(_, _, _) ->
    ok.


encoded_settings(Spec, Test) ->
    case cmencode:encode(Spec) of 
        {ok, S} -> 
            case maps:get(opts, Test, undef) of 
                undef -> {ok, S};
                Opts -> 
                    {ok, maps:merge(S, Opts)}
            end;
        Other ->
            Other
    end.

encoded_facts(#{ facts := Facts }, Settings) ->
    cmencode:encode(Facts, #{ settings => Settings }).

start_test([], Data) ->
    {finished, Data#{ finished => cmkit:now() }};

start_test([#{ scenarios := Scenarios}=T|Rem], Data) ->
    start_scenario(T, Scenarios, Data#{ test => T,
                                        tests => Rem}).

start_scenario(_, [], #{ tests := [] } = Data) ->
    {finished, Data#{ finished => cmkit:now() }};

start_scenario(_, [], #{ tests := Tests }=Data) ->
    start_test(Tests, Data);

start_scenario(#{ name := TestTitle }=Test, [#{title := ScenarioTitle}=S|Rem], #{ facts := Facts,
                                                                                  result := R,
                                                                                  fail := Fail,
                                                                                  settings := #{ value := Settings } }=Data) ->
    case cmtest_util:start(Test, S, Facts, Settings, self() ) of 
        {ok, Pid} ->

            Data2 = Data#{ 
                           pid => Pid,
                           scenarios => Rem,
                           started => cmkit:now()
                         },
            cmtest_scenario:next(Pid),
            {ok, Data2};
        {error, E} ->

            ScenarioResult = #{ test => TestTitle,
                                scenario => ScenarioTitle,
                                status => fail,
                                elapsed => 0,
                                failure => E,
                                steps => [] },

            Data2 = Data#{ current => [],
                           result => [ScenarioResult|R],
                           fail => Fail + 1 },

            start_scenario(Test, Rem, Data2)
    end;

start_scenario(_, _, Data) ->
    {finished, Data#{ finished => cmkit:now() }}.

stop_scenario(#{ pid := Pid }=Data) ->
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

    Report = #{ timestamp => cmkit:now(),
                query => Q,
                settings => SettingsName,
                status => ok,
                scenarios => Stats,
                seconds => Secs,
                result => Result},

    Failures = [ #{ scenario => Scenario,
                    failure => Failure } || #{ scenario := Scenario, 
                                               failure := Failure,
                                               status := fail } <- R ],

    cmkit:S({cmtest, Q, SettingsName, Stats, Failures, Secs}),
    save_report(Report, Data),
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


save_report(_, #{ test := #{ config := #{ report := false }}}) ->
    cmkit:log({cmtest, report, skipped});

save_report(#{ query := Query } = R, _) -> 
    Id = cmkit:uuid(),
    Pairs = cmtest_util:report_entries(R#{ id => Id }), 
    Res = cmdb:put(tests, Pairs),
    cmkit:log({cmtest, report, Query, Id, length(Pairs), Res}),
    Res.

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
