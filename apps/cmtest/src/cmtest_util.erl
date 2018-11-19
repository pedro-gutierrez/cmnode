-module(cmtest_util).
-export([hash/1,
         hash/2,
         hash/3,
         start/5,
         scenarios_by_tag/2,
         steps/2,
         run/3,
         close/2, 
         report_entries/1,
         report_sort_fun/2,
         printable/1]).

report_entries(#{ id := Id, 
                  query := Q, 
                  result := Result, 
                  timestamp := TStamp,
                  status := Status,
                  seconds := Seconds,
                  scenarios := #{ success := Passed,
                                  fail := Failed,
                                  total := Total }}=R) ->
    Summary = report_summary(R),
    Scenarios = lists:map(fun report_scenario_summary/1, Result),
    QHash = hash(Q),
    H = #{ label => TStamp,
           status => Status,
           duration => Seconds,
           passed => Passed,
           failed => Failed,
           total => Total },

    with_scenarios_entries(R, [{report, is, Id, Summary},
                               {reports, QHash, Id, Id},
                               {reports, all, TStamp, Id},
                               {history, QHash, TStamp, H},
                               {scenarios, all, Id, Scenarios}]).

report_summary(R) ->
    maps:with([ id,
                timestamp,
                query,
                settings,
                seconds,
                status,
                scenarios], R).

report_scenario_summary(S) ->
    maps:with([ elapsed,
                scenario,
                status,
                test ], S).


with_scenarios_entries(#{ id := Id,
                          result := Scenarios,
                          query := Test,
                          timestamp := TStamp}, Entries) ->
    lists:foldl(fun(#{ scenario := Title,
                       status := Status,
                       elapsed := Elapsed } = S, E0) ->
                        SHash = hash(Test, Title),
                        H = #{ label => TStamp, 
                               duration => Elapsed,
                               status => Status },
                        E1 = [{scenario, Id, Title, S},
                              {history, SHash, TStamp, H}],
                        with_steps_entries(S#{ timestamp => TStamp}, E1++E0)
                end, Entries, Scenarios).

with_steps_entries(#{ steps := Steps,
                     scenario := Scenario,
                     timestamp := TStamp,
                     test := Test }, Entries) ->

    lists:foldl(fun(#{ millis := Millis,
                       status := Status,
                       step := #{ title := Step }}, E0) ->
                    SHash = hash(Test, Scenario, Step),
                    H = #{ label => TStamp, 
                           status => Status,
                           duration => Millis },
                    [{history, SHash, TStamp, H}|E0]
                end, Entries, Steps).




hash(Test) -> cmkit:hash(cmkit:to_bin(Test)).
hash(Test, Scenario) -> cmkit:hash(cmkit:bin_join([cmkit:to_bin(Test), Scenario])).
hash(Test, Scenario, Step) -> cmkit:hash(cmkit:bin_join([cmkit:to_bin(Test), Scenario, Step])).






start(Test, Scenario, Facts, Settings, Runner) ->
    case cmtest_util:steps(Scenario, Test) of 
        {ok, Steps} -> 
            case cmtest_scenario_sup:start(Test, Scenario, Steps, Facts, Settings, Runner) of 
                {ok, Pid} ->
                    {ok, Pid};
                Other ->
                    Other
            end;
        Other -> 
            Other
    end.


scenarios_by_tag(Tag, Scenarios) ->
    {ok, lists:filter(fun(#{ tags := Tags}) ->
                              lists:member(Tag, Tags)
                      end, Scenarios)}.

steps(#{ steps := Steps,
         backgrounds := Backgrounds }, Test) ->
    Procs = index_procedures(Test),
    IndexedReusableSteps = index_reusable_steps(Test),
    case resolve_backgrounds(Backgrounds, Test, IndexedReusableSteps, Procs) of 
        {ok, Resolved } ->
            Steps3 = lists:flatten(lists:map(fun(B) -> 
                                        maps:get(steps, B)
                                    end, Resolved)) ++ Steps,
            case resolve_procedures(Steps3, Procs, []) of 
                {ok, Steps4} -> 
                    resolve_steps(Steps4, IndexedReusableSteps, Procs);
                Other -> 
                    Other
            end;
        Other -> Other
    end.

index_reusable_steps(#{ steps := Steps }) ->
    lists:foldl(fun(#{ title := Title }=Spec, Acc) ->
                    Acc#{ Title => Spec }
                end, #{}, Steps).

index_procedures(#{ procedures := Procs }) ->
    lists:foldl(fun(#{ name := Name,
                       spec := Spec } = Spec0, Index) ->
                    Spec1 = #{ spec => Spec },
                    Spec2 = case maps:get(as, Spec0, undef) of
                                undef -> Spec1;
                                As -> 
                                    Spec1#{ as => As }
                            end,

                    Index#{ Name => Spec2 }
                end, #{}, Procs);

index_procedures(_) -> #{}.

resolve_procedures([], _, Steps) -> {ok, lists:reverse(Steps)};
resolve_procedures([S|Rem], Procs, Steps) ->
    case resolve_procedure(S, Procs) of 
        {ok, S2} -> 
            resolve_procedures(Rem, Procs, [S2|Steps]);
        Other -> 
            Other
    end.

resolve_procedure(#{ type := procedure,
                       name := ProcName }=S, Procs) -> 
    case maps:get(ProcName, Procs, undef) of
        undef ->
            {error, #{ error => missing_procedure,
                       name => ProcName }};
        #{ spec := Spec } = Spec0 ->
            S1 = maps:without([name], S#{ spec => Spec }),
            S2 = case maps:get(as, Spec0, undef) of
                     undef ->
                         S1;
                     As ->
                         S1#{ as => As }
                 end,
            {ok, S2}
    end;

resolve_procedure(S, _Procs) -> {ok, S}.

resolve_backgrounds(Backgrounds, Test, ReusableSteps, Procs) ->
    resolve_backgrounds(Backgrounds, Test, ReusableSteps, Procs, []).

resolve_backgrounds([], _, _, _, Out) -> {ok, lists:reverse(Out)};
resolve_backgrounds([#{ id := BId, title := Title }=BRef|Rem], #{ backgrounds := Backgrounds }=Test, ReusableSteps, Procs, Out) ->
    R = case maps:get(BId, Backgrounds, undef) of 
            undef -> 
                case maps:get(Title, ReusableSteps, undef) of 
                    undef -> undef;
                    Step -> #{ title => Title,
                               steps => [Step] }
                end;

            Other -> 
                Other
        end,

    case R of 
        undef -> 
            {error, #{ error => missing_background_or_step,
                       name => BRef }};
        #{ steps := Steps } = Resolved ->
            case resolve_procedures(Steps, Procs, []) of
                {ok, Steps2} ->
                    case resolve_steps(Steps2, ReusableSteps, Procs) of 
                        {ok, Steps3} -> 
                            resolve_backgrounds(Rem, Test, ReusableSteps, Procs, [Resolved#{ steps => Steps3 }|Out]);
                        Other2 -> 
                            Other2
                    end;
                Other2 ->
                    Other2
            end
    end.

resolve_steps(Steps, ReusableSteps, Procs) -> 
    resolve_steps(Steps, ReusableSteps, Procs, []).

resolve_steps([], _, _, Out) -> {ok, lists:reverse(Out)};
resolve_steps([#{ ref := Title}|Rem], ReusableSteps, Procs, Out)  ->
    case resolve_step(Title, ReusableSteps, Procs) of 
        {ok, S} ->
            resolve_steps(Rem, ReusableSteps, Procs, [S|Out]);
        Other ->
            Other
    end;

resolve_steps([#{ type := parallel, 
                  spec := #{ ref := Title } }=Spec|Rem], ReusableSteps, Procs, Out) ->
    case resolve_step(Title, ReusableSteps, Procs) of 
        {ok, S} ->
            resolve_steps(Rem, ReusableSteps, Procs, [Spec#{ spec => S }|Out]);
        Other ->
            Other
    end;

resolve_steps([#{ spec := _ }=Spec|Rem], ReusableSteps, Procs, Out) ->
    resolve_steps(Rem, ReusableSteps, Procs, [Spec|Out]).


resolve_step(Title, ReusableSteps, Procs) ->
    case maps:get(Title, ReusableSteps, undef) of 
        undef -> 
            {error, #{ error => missing_step,
                       name => Title }};
        S ->
            resolve_procedure(S, Procs)
    end.

world_with_conn(App, Props, #{ conns := Conns }=World) ->
    Conns2 = Conns#{ App => maps:merge(Props, #{ name => App,
                                                 app => App })},
    World#{ conns => Conns2 }.

run(#{ type := _ }, _, #{ retries := #{ left := 0}}) ->
    {error, #{ error => max_retries_reached, 
               info => none }};

run(#{ type := probe, 
       spec := #{ connection := ConnSpec,
                  status := Status  }}, Settings, #{ conns := Conns }=World) ->
    
    case cmencode:encode(ConnSpec, World#{ settings => Settings}) of 
        {ok, Name} -> 
            case maps:get(Name, Conns, undef) of
                undef ->
                    {error, #{ error => not_such_connection,
                               info => Name}};
                #{ status := Status } ->
                    {ok, World};
                _ ->
                    retry
            end;
        Other -> 
            Other
    end;

run(#{ type := disconnect, 
       spec := Spec }, Settings, #{ conns := Conns }=World) ->
    
    case cmencode:encode(Spec, #{ settings => Settings,
                                  world => World}) of 
        {ok, #{ app := Conn }} ->
            case maps:get(Conn, Conns, undef) of
                undef ->
                    {error, #{ error => not_such_connection,
                               info => Conn}};

                #{ class := websocket, pid := Pid } ->
                    cmkit:log({cmtest, disconnecting, Conn, Pid}),
                    ok = cmwsc:stop(Pid),
                    cmkit:log({cmtest, disconnected, Conn, Pid}),
                    {ok, World}
            end;
        Other -> 
            {error, #{ error => encode_error,
                       info => Other}}
    end;

run(#{ type := expect, 
       spec := Spec }, Settings, #{ data := Data }=World) ->
    In = World#{ settings => Settings },
    case cmencode:encode(Spec, In) of 
        {ok, false} -> retry;
        {ok, true} -> 
            {ok, World};
        {ok, Encoded} ->
            {ok, World#{ data => maps:merge( Data, Encoded)}};
        Other -> 
            Other
    end;

run(#{ type := recv, 
       from := ConnSpec,
       spec := Spec } = RecvSpec, Settings, #{ data := Data,
                                               conns := Conns } = World ) ->

    In = World#{ settings => Settings },
    case cmencode:encode(ConnSpec, In) of
        {ok, Name} ->
            case maps:get(Name, Conns, undef) of 
                undef -> 
                    {error, #{ error => no_such_connection,
                               info => Name}};
                #{ inbox := Inbox } ->
                    Spec0 = #{ type => first,
                               spec => Spec },
                    case cmdecode:decode(Spec0, Inbox, In) of 
                        {ok, Decoded} ->
                            case maps:get(as, RecvSpec, undef) of 
                                undef -> 
                                    {ok, World };
                                Key when is_atom(Key) ->
                                    {ok, World#{ data => maps:merge(Data, #{ Key => Decoded})}};

                                RememberSpec when is_map(RememberSpec) -> 
                                    case cmencode:encode(RememberSpec, Decoded) of 
                                        {ok, Data2} ->
                                            {ok, World#{ data => maps:merge(Data, Data2) }};

                                        {error, E} ->
                                            {error, #{ error => encode_error,
                                                       info => E }} 
                                    end
                            end;
                        no_match -> 
                            retry
                    end
            end;
        Other -> 
            Other
    end;

run(#{ type := kube } = Spec,  Settings, #{ data := Data }=World ) ->

    In = World#{ settings => Settings },
    case cmencode:encode(Spec, In) of 
        {ok, Res} ->
            Key = maps:get(as, Spec, latest),
            {ok, World#{ data => Data#{ Key => Res }}};
        Other ->
            Other
    end;

run(#{ type := procedure,
       params := Params,
       spec := Spec }=Spec0, Settings, #{ data := Data }=World) ->
        
        In0 = World#{ settings => Settings },
        case cmencode:encode(Params, In0) of 
            {ok, EncodedParams} ->
                In = In0#{ params => EncodedParams },    
                case cmencode:encode(Spec, In) of 
                    {ok, #{ connection := #{ name := Name }=Conn }} ->
                        {ok, world_with_conn(Name, Conn, World)};
                    {ok, Res} ->
                        case maps:get(as, Spec0, undef) of 
                            undef -> 
                                {ok, World};

                            AsSpec -> 
                                case cmencode:encode(AsSpec, In) of 
                                    {ok, As} ->
                                        Key = cmkit:to_atom(As),
                                        {ok, World#{ data => Data#{ Key => Res }}};
                                    {error, E } -> 
                                        {error, #{ error => encode_error,
                                                   phase => as,
                                                   info => E }}
                                end
                        end;
                    {error, E} ->
                        {error, #{ error => encode_error,
                                   phase => procedure,
                                   info => E }}
                end;
            {error, E} -> 
                {error, #{ error => encode_error,
                           phase => params,
                           info => E }}
        end;

run(#{ type := connect } = Spec, Settings, World ) ->
    case cmencode:encode(Spec,  World#{ settings => Settings }) of
        {ok, #{ connection := #{ name := Name } = Conn}} ->
            {ok, world_with_conn(Name, Conn, World)};
        {ok, Other} -> 
            {error, #{ error => not_a_connection,
                       info => Other }};
        Other -> 
            Other
    end;

run(#{ type := send } = Spec, Settings, World ) ->
    case cmencode:encode(Spec, World#{ settings => Settings }) of
        {ok, #{ connection := #{ name := Name } = Conn}} ->
            {ok, world_with_conn(Name, Conn, World)};
        {ok, Other} -> 
            {error, #{ error => not_a_connection,
                       info => Other }};
        Other -> 
            Other
    end;

run(#{ type := merge, 
       spec := _ } = Spec, Settings, #{ data := Data } =World) ->
   case cmencode:encode(Spec, #{ data =>  Data }, Settings) of 
       {ok, Data2} -> 
           {ok, World#{ data => Data2}};
       Other -> 
           Other
   end;

run(#{ type := set,
       spec := Spec}, Settings, #{ data := Data } =World) ->
   case cmencode:encode(Spec, World#{ settings => Settings }, Settings) of 
       {ok, Data2} -> 
           {ok, World#{ data => maps:merge(Data, Data2)}};
       Other -> 
           Other
   end;

run(#{ type := parallel,
       count := CountSpec,
       spec := Spec0 }, Settings, #{ data := _Data }=World) ->
    In = World#{ settings => Settings },
    case cmencode:encode(CountSpec, In) of
        {ok, Count} ->
            FuncCall = {?MODULE, run, [Spec0, Settings, World]},
            FuncCalls = lists:map(fun(_) -> FuncCall end, lists:seq(1, Count)),
            _Res = rpc:parallel_eval(FuncCalls),
            {ok, World};
        Other ->
            Other
    end;

run(Spec, Settings, #{ data := Data }=World) ->
    In = World#{ settings => Settings },
    case encode_alias(Spec, In) of 
        {ok, Alias} ->
            case cmencode:encode(Spec, In) of 
                {ok, Encoded} ->
                    { ok, World#{ data => Data#{ Alias => Encoded }}};
                Other -> 
                    Other
            end;
        Other ->
            Other
    end.


encode_alias(#{ as := AliasSpec }, In) ->
    case cmencode:encode(AliasSpec, In) of 
        {ok, As} when is_binary(As) ->
            {ok, cmkit:to_atom(As)};
        Other ->
            Other
    end;

encode_alias(_, _) -> {ok, latest }.

close(#{ conns := _Conns }, _Pid) ->
    ok.


report_sort_fun(#{ timestamp := T1}, #{ timestamp := T2}) ->
    T1 > T2.

printable(Map) ->
    cmkit:printable(128, Map).
