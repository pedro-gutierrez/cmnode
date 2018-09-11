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
         status/0
        ]).

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
                {ok, Spec} -> 
                    cmtest_runner:run(Spec#{ id => Id,
                                             opts => Opts }, SettingsSpec);
                Other -> Other
            end;
        Other -> Other
    end.


run(Test, Settings) ->
    case cmconfig:settings(Settings) of 
        {ok, SettingsSpec } -> 
            case cmconfig:test(Test) of
                {ok, Spec} -> 
                    cmtest_runner:run(Spec, SettingsSpec);
                Other -> Other
            end;
        Other -> Other
    end.

run(Test, Settings, Tag) ->
    case cmconfig:settings(Settings) of 
        {ok, SettingsSpec } -> 
            case cmconfig:test(Test) of
                {ok, #{ scenarios := Scenarios }=Spec} ->
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

report(Id) ->
    case cmdb:first(tests, reports, has_id, Id) of 
        {ok, R} ->
            {ok, sanitized_report(R)};
        not_found ->
            {error, not_found};
        Other -> 
            {error, Other}
    end.

reports() -> reports(1).

reports(Days) ->
    Ids = lists:foldl(fun(D, Set) ->
                              Key = cmcalendar:to_bin(D, date),
                              case cmdb:all(tests, reports, Key) of
                                  {ok, Ids} ->
                                      sets:union(Set, sets:from_list(Ids));
                                  _ -> 
                                      Set
                              end
                      end, sets:new(), cmcalendar:last({Days, days})),

    Reports = lists:foldl(fun(Id, Reports) ->
                                  case cmdb:first(tests, reports, has_id,Id) of 
                                      {ok, R} -> [R|Reports];
                                      _ -> Reports
                                  end                
                          end, [], sets:to_list(Ids)),
    
    Reports2 = lists:sort(fun cmtest_util:report_sort_fun/2, Reports),
    Reports3 = cmkit:top(20, Reports2),
    {ok, Reports3}.


sanitized_report(#{ result := Result }=R) ->
    R#{ result => sanitized_report_result(Result) }.

sanitized_report_result(Items) -> 
    lists:map(fun(#{ status := success }=I) -> I;
                 (#{ status := fail,
                     failure := F }=I) -> 
                      I#{ failure => sanitized_failure(F) } 
              end, Items).

sanitized_failure(#{ reason := R }=F) -> 
    F#{ reason => sanitized_failure_reason(R) };

sanitized_failure(F) ->  F.

sanitized_failure_reason(#{ info := Info }=R) ->
    R#{ info => sanitized_failure_reason_info(Info) };

sanitized_failure_reason(R) -> R.

sanitized_failure_reason_info(#{ data := _ }=I) ->
    maps:without([data], I);

sanitized_failure_reason_info(I) -> I.

status() -> 
    cmqueue:status(tests_queue).
