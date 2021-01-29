-module(cmtest).
-export([
         subscribe/1,
         run/1,
         run/2, 
         run/3,
         stop/1,
         cancel/1,
         clear/0, 
         schedule/2,
         schedule/3, 
         reports/0, 
         reports/1, 
         report/1,
         report_scenario/2,
         status/0,
         test_history/1,
         scenario_history/2,
         step_history/3
        ]).
-define(REL, is).

subscribe(SessionId) ->
    cmqueue:subscribe(tests_queue, tests, SessionId).

cancel(Id) ->
    cmqueue:cancel(tests_queue, Id).

run(#{ id := Id,
       test := Test,
       settings := Settings,
       opts := Opts }) -> 

    case cmconfig:settings(Settings) of 
        {ok, SettingsSpec} -> 
            case cmconfig:test(Test) of
                {ok, Spec0} -> 
                    Spec = cmtest_util:with_includes(Spec0),
                    cmtest_runner:run(Spec#{ id => Id,
                                             opts => Opts }, SettingsSpec);
                Other -> Other
            end;
        Other -> Other
    end;

run(Test) ->
    run(Test, local).

run(Test, Settings) ->
    case cmconfig:settings(Settings) of 
        {ok, SettingsSpec } -> 
            case cmconfig:test(Test) of
                {ok, Spec0} -> 
                    Spec = cmtest_util:with_includes(Spec0),
                    cmtest_runner:run(Spec, SettingsSpec);
                Other -> Other
            end;
        Other -> Other
    end.

run(Test, Settings, Tag) ->
    case cmconfig:settings(Settings) of 
        {ok, SettingsSpec } -> 
            case cmconfig:test(Test) of
                {ok, #{ scenarios := Scenarios }=Spec0} ->
                    Spec = cmtest_util:with_includes(Spec0),
                    {ok, SSpecs} = cmtest_util:scenarios_by_tag(Tag, Scenarios),
                    cmtest_runner:run(Spec, SSpecs, SettingsSpec);
                Other -> Other
            end;
        Other -> Other
    end.

stop(Id) ->
    cmtest_runner:stop(Id).

clear() ->
    cmqueue:clear(tests_queue). 

schedule(T, S) -> 
    schedule(T, S, #{}).

schedule(T, S, Opts) ->
    case cmconfig:settings(S) of 
        {ok, _ } -> 
            case cmconfig:test(T) of
                {ok, _ } ->
                    cmqueue:schedule(tests_queue, queue_job(T, S, Opts) ); 
                Other -> Other
            end;
        Other -> Other
    end.

queue_job(T, S, Opts) ->
    Now = cmkit:now(),

    #{ id => Now,
       timestamp => Now,
       info => #{ test => T,
                  settings => S,
                  info => <<"Not started yet">> },
       spec => #{ start => {cmtest, run, [#{ id => Now,
                                             test => T,
                                             settings => S,
                                             opts => Opts }]},

                  stop => {cmtest, stop, [Now]}}
     }.

report_scenario(Id, Title) ->
    case cmdb:get(tests, scenario, Id, Title) of 
        [{_, _, _, S}] ->
            {ok, S};
        [] ->
            {error, not_found};
        Other -> 
            {error, Other}
    end.

reports() -> reports(1).

report_summary(Id) ->
    case cmdb:get(tests, report, ?REL, Id) of 
        [{_, _, _, R}] ->
            {ok, R};
        []->
            {error, not_found};
        Other -> 
            {error, Other}
    end.

report_scenarios_summary(Id) ->
    case cmdb:get(tests, scenarios, all, Id) of 
        [{_, _, _, S}] ->
            {ok, S};
        []->
            {error, not_found};
        Other -> 
            {error, Other}
    end.

report(Id) -> 
    case report_summary(Id) of 
        {ok, R} ->
            case report_scenarios_summary(Id) of 
                {ok, S} ->
                    {ok, R#{ result => S }};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

reports(Days) ->
    Since = cmcalendar:to_epoch(cmcalendar:back({Days, days})) *1000,
    case cmdb:get(tests, reports, all, Since, cmkit:now()) of 
        Entries when is_list(Entries) ->
            Ids = [Id || {_, _, _, Id} <- Entries],
            Reports = lists:foldl(fun(Id, R0) ->
                                          case report_summary(Id) of 
                                              {ok, R} ->
                                                  [R|R0];
                                              _ -> R0
                                          end                
                                  end, [], Ids),
            {ok, Reports};
        Other ->
            Other
    end.

status() -> 
    cmqueue:status(tests_queue).

test_history(Test, Days) ->
    Hash = cmtest_util:hash(Test),
    history(Hash, Days, [duration, failed, passed]).

test_history(Test) -> test_history(Test, 7).

scenario_history(Test, Scenario) ->
    scenario_history(Test, Scenario, 7).

scenario_history(Test, Scenario, Days) ->
    Hash = cmtest_util:hash(Test, Scenario),
    history(Hash, Days, [duration]).

step_history(Test, Scenario, Step) ->
    step_history(Test, Scenario, Step, 7).

step_history(Test, Scenario, Step, Days) ->
    Hash = cmtest_util:hash(Test, Scenario, Step),
    history(Hash, Days, [duration]).

history(Query, Days, Series) ->
    Since = cmcalendar:to_epoch(cmcalendar:back({Days, days})) *1000,
    case cmdb:get(tests, history, Query, Since, cmkit:now()) of
        Entries when is_list(Entries) ->
            Acc = lists:foldl(fun(S, Acc0) ->
                                      Acc0#{ S => []}
                              end, #{}, Series),
            Acc2 = Acc#{ labels => [], series => Acc },
            History = lists:foldr(fun({_, _, _, #{ label := L }=H}, #{ labels := Labels,
                                                                       series := AllSeries0 }=Acc0) ->
                                          AllSeries1 = lists:foldl(fun(S, A0) ->
                                                                           V = maps:get(S, H),
                                                                           S0 = maps:get(S, A0),
                                                                           A0#{ S => [V|S0] } 
                                                                   end, AllSeries0, Series),
                                          Acc0#{ labels => [L|Labels], series => AllSeries1 }
                                  end, Acc2, Entries),
            {ok, History};
        Other ->
            Other
    end.
