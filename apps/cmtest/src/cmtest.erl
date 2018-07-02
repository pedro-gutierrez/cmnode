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
    case cmdb:get(tests, {report, Id}) of 
        {ok, [R] } ->
            {ok, R};
        {ok, []} ->
            {error, not_found};
        {ok, [_|_]} -> 
            {error, too_many_reports};
        Other -> 
            {error, Other}
    end.

reports() -> reports(1).

reports(Days) ->
    Ids = lists:foldl(fun(D, Set) ->
                              Key = cmcalendar:to_bin(D, date),
                              case cmdb:get(tests, {report, Key}) of
                                  {ok, Ids} ->
                                      sets:union(Set, sets:from_list(Ids));
                                  _ -> 
                                      Set
                              end
                      end, sets:new(), cmcalendar:last({Days, days})),

    Reports = lists:foldl(fun(Id, Reports) ->
                                  case cmdb:get(tests, {report, Id}) of 
                                      {ok, [R]} -> [R|Reports];
                                      _ -> Reports
                                  end                
                          end, [], sets:to_list(Ids)),
    
    Reports2 = lists:sort(fun cmtest_util:report_sort_fun/2, Reports),
    Reports3 = cmkit:top(20, Reports2),
    {ok, Reports3}.



status() -> 
    cmqueue:status(tests_queue).
