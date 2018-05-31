-module(cmtest).
-export([run/0, run/1, run/2, clear/0, schedule/0, schedule/1, reports/0, reports/1, report/1, status/0]).

run() -> 
    cmtest_runner:run(cmconfig:tests()).

run(Test) ->
    case cmconfig:test(Test) of
        {ok, Spec} -> 
            cmtest_runner:run(Spec);
        Other -> Other
    end.

run(Test, Tag) ->
    case cmconfig:test(Test) of
        {ok, #{ scenarios := Scenarios }=Spec} ->
            {ok, SSpecs} = cmtest_util:scenarios_by_tag(Tag, Scenarios),
            cmtest_runner:run(Spec, SSpecs);
        Other -> Other
    end.

clear() ->
    cmqueue:clear(tests_queue). 

schedule() ->
    [ cmqueue:schedule(tests_queue, queue_job(T) ) ||
        #{ name := T } <- cmconfig:tests() ].

schedule(T) ->
    case cmconfig:test(T) of
        {ok, _ } ->
            cmqueue:schedule(tests_queue, queue_job(T) ); 
        Other -> Other
    end.

queue_job(T) ->
    Now = cmkit:now(),

    #{ id => Now,
       timestamp => Now,
       type => test,
       name => T,
       spec => {cmtest, run, [T]}
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
    {ok, Reports}.


status() -> 
    cmqueue:status(tests_queue).
