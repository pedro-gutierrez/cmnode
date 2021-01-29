-module(cmtest_util).

-export([hash/1, hash/2, hash/3, start/5, scenarios_by_tag/2, steps/2, run/3, close/2,
         report_entries/1, report_sort_fun/2, with_includes/1, printable/1]).

report_entries(#{id := Id,
                 query := Q,
                 result := Result,
                 timestamp := TStamp,
                 status := Status,
                 seconds := Seconds,
                 scenarios :=
                     #{success := Passed,
                       fail := Failed,
                       total := Total}} =
                   R) ->
    Summary = report_summary(R),
    Scenarios = lists:map(fun report_scenario_summary/1, Result),
    QHash = hash(Q),
    H = #{label => TStamp,
          status => Status,
          duration => Seconds,
          passed => Passed,
          failed => Failed,
          total => Total},

    with_scenarios_entries(R,
                           [{report, is, Id, Summary},
                            {reports, QHash, Id, Id},
                            {reports, all, TStamp, Id},
                            {history, QHash, TStamp, H},
                            {scenarios, all, Id, Scenarios}]).

report_summary(R) ->
    maps:with([id, timestamp, query, settings, seconds, status, scenarios], R).

report_scenario_summary(S) ->
    maps:with([elapsed, scenario, status, test], S).

with_scenarios_entries(#{id := Id,
                         result := Scenarios,
                         query := Test,
                         timestamp := TStamp},
                       Entries) ->
    lists:foldl(fun(#{scenario := Title,
                      status := Status,
                      elapsed := Elapsed} =
                        S,
                    E0) ->
                        SHash = hash(Test, Title),
                        H = #{label => TStamp,
                              duration => Elapsed,
                              status => Status},
                        E1 = [{scenario, Id, Title, S}, {history, SHash, TStamp, H}],
                        with_steps_entries(S#{timestamp => TStamp}, E1 ++ E0)
                end,
                Entries,
                Scenarios).

with_steps_entries(#{steps := Steps,
                     scenario := Scenario,
                     timestamp := TStamp,
                     test := Test},
                   Entries) ->
    lists:foldl(fun(#{millis := Millis,
                      status := Status,
                      step := #{title := Step}},
                    E0) ->
                        SHash = hash(Test, Scenario, Step),
                        H = #{label => TStamp,
                              status => Status,
                              duration => Millis},
                        [{history, SHash, TStamp, H} | E0]
                end,
                Entries,
                Steps).

hash(Test) ->
    cmkit:hash(
      cmkit:to_bin(Test)).

hash(Test, Scenario) ->
    cmkit:hash(
      cmkit:bin_join([cmkit:to_bin(Test), Scenario])).

hash(Test, Scenario, Step) ->
    cmkit:hash(
      cmkit:bin_join([cmkit:to_bin(Test), Scenario, Step])).

with_debug(Debug, Steps) when is_list(Steps) ->
    lists:map(fun(S) -> with_debug(Debug, S) end, Steps);
with_debug(_, #{type := object} = S0) ->
    S0;
with_debug(Debug, #{spec := S1} = S0) ->
    S0#{debug => Debug, spec => with_debug(Debug, S1)};
with_debug(Debug, S) when is_map(S) ->
    S#{debug => Debug};
with_debug(_, S) ->
    S.

with_includes(#{include := Includes} = Spec) ->
    with_includes(Includes, Spec);
with_includes(Spec) ->
    Spec.

with_includes([], Spec) ->
    Spec;
with_includes([T | Rem], Spec) ->
    case cmconfig:test(T) of
        {ok, Spec0} ->
            with_includes(Rem, merged_specs(Spec0, Spec));
        {error, E} ->
            cmkit:warning({cmtest, include, T, E}),
            with_includes(Rem, Spec)
    end.

merged_specs(Spec0, Spec) ->
    lists:foldl(fun(MergeFun, TmpSpec) -> MergeFun(Spec0, TmpSpec) end,
                Spec,
                [fun merged_backgrounds/2,
                 fun merged_procedures/2,
                 fun merged_steps/2,
                 fun merged_facts/2]).

merged_backgrounds(#{backgrounds := B0}, #{backgrounds := B} = Spec) ->
    Spec#{backgrounds => maps:merge(B0, B)};
merged_backgrounds(_, Spec) ->
    Spec.

merged_procedures(#{procedures := P0}, #{procedures := P} = Spec) ->
    Spec#{procedures => lists:append(P, P0)};
merged_procedures(_, Spec) ->
    Spec.

merged_steps(#{steps := S0}, #{steps := S} = Spec) ->
    Spec#{steps => lists:append(S, S0)};
merged_steps(_, Spec) ->
    Spec.

merged_facts(#{facts := F0}, #{facts := F} = Spec) ->
    Spec#{facts => cmkit:merge(F0, F)};
merged_facts(_, Spec) ->
    Spec.

start(Test, #{debug := Debug} = Scenario, Facts, Settings, Runner) ->
    case cmtest_util:steps(Scenario, Test) of
        {ok, Steps} ->
            Steps2 = with_debug(Debug, Steps),
            case cmtest_scenario_sup:start(Test, Scenario, Steps2, Facts, Settings, Runner) of
                {ok, Pid} ->
                    {ok, Pid};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

scenarios_by_tag(Tag, Scenarios) ->
    {ok, lists:filter(fun(#{tags := Tags}) -> lists:member(Tag, Tags) end, Scenarios)}.

steps_from_background(#{steps := Steps}) ->
    Steps.

steps_from_backgrounds(Backgrounds) when is_list(Backgrounds) ->
    lists:flatten(
      lists:map(fun steps_from_background/1, Backgrounds));
steps_from_backgrounds(Other) ->
    cmkit:warning({cmtest, steps_from_backgrounds, unexpected, Other}),
    [].

steps(#{steps := Steps, backgrounds := Backgrounds}, Test) ->
    Procs = index_procedures(Test),
    IndexedReusableSteps = index_reusable_steps(Test),
    case resolve_backgrounds(Steps,
                             Test,
                             IndexedReusableSteps,
                             Procs,
                             fun maybe_resolve_background/2)
    of
        {ok, Resolved1} ->
            case resolve_backgrounds(Backgrounds,
                                     Test,
                                     IndexedReusableSteps,
                                     Procs,
                                     fun enforce_resolve_background/2)
            of
                {ok, Resolved2} ->
                    Steps3 = steps_from_backgrounds(Resolved2) ++ steps_from_backgrounds(Resolved1),
                    case resolve_procedures(Steps3, Procs, []) of
                        {ok, Steps4} ->
                            {ok, Steps4};
                                                %resolve_steps(Steps4, IndexedReusableSteps, Procs);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

index_reusable_steps(#{steps := Steps}) ->
    lists:foldl(fun(#{title := Title} = Spec, Acc) -> Acc#{Title => Spec} end, #{}, Steps).

index_procedures(#{procedures := Procs}) ->
    lists:foldl(fun(#{name := Name, spec := Spec} = Spec0, Index) ->
                        Spec1 = #{spec => Spec},
                        Spec2 =
                            case maps:get(as, Spec0, undef) of
                                undef -> Spec1;
                                As -> Spec1#{as => As}
                            end,

                        Index#{Name => Spec2}
                end,
                #{},
                Procs);
index_procedures(_) ->
    #{}.

resolve_procedures([], _, Steps) ->
    {ok, lists:reverse(Steps)};
resolve_procedures([S | Rem], Procs, Steps) ->
    case resolve_procedure(S, Procs) of
        {ok, S2} ->
            resolve_procedures(Rem, Procs, [S2 | Steps]);
        Other ->
            Other
    end.

resolve_procedure(#{type := procedure, name := ProcName} = S, Procs) ->
    case maps:get(ProcName, Procs, undef) of
        undef ->
            {error, #{error => missing_procedure, name => ProcName}};
        #{spec := Spec} = Spec0 ->
            S1 = maps:without([name], S#{spec => Spec}),
            S2 = case maps:get(as, Spec0, undef) of
                     undef ->
                         S1;
                     As ->
                         S1#{as => As}
                 end,
            {ok, S2}
    end;
resolve_procedure(S, _Procs) ->
    {ok, S}.

resolve_backgrounds(Backgrounds, Test, ReusableSteps, Procs, ResolveFun) ->
    resolve_backgrounds(Backgrounds, Test, ReusableSteps, Procs, ResolveFun, []).

resolve_backgrounds([], _, _, _, _, Out) ->
    {ok, lists:reverse(Out)};
resolve_backgrounds([Ref | Rem],
                    #{backgrounds := Backgrounds} = Test,
                    ReusableSteps,
                    Procs,
                    ResolveFun,
                    Out) ->
    R = case lookup_step(Ref, Backgrounds, ReusableSteps) of
            {Title, #{steps := Steps0}} ->
                #{title => Title, steps => Steps0};
            {Title, Step} ->
                #{title => Title, steps => [Step]};
            Other ->
                Other
        end,
    case R of
        {error, E} ->
            {error, E};
        _ ->
            case ResolveFun(R, Ref) of
                {ok, #{steps := Steps} = Resolved} ->
                    case resolve_procedures(Steps, Procs, []) of
                        {ok, Steps2} ->
                            case resolve_steps(Steps2, ReusableSteps, Procs) of
                                {ok, Steps3} ->
                                    resolve_backgrounds(Rem,
                                                        Test,
                                                        ReusableSteps,
                                                        Procs,
                                                        ResolveFun,
                                                        [Resolved#{steps => Steps3} | Out]);
                                Other2 ->
                                    Other2
                            end;
                        Other2 ->
                            Other2
                    end;
                Other2 ->
                    Other2
            end
    end.

lookup_step(#{id := Bid, title := Title}, Backgrounds, ReusableSteps) ->
    case maps:get(Bid, Backgrounds, undef) of
        undef ->
            lookup_step(Title, Backgrounds, ReusableSteps);
        Background ->
            {Title, Background}
    end;
lookup_step(#{title := Title}, Backgrounds, ReusableSteps) ->
    lookup_step(Title, Backgrounds, ReusableSteps);
lookup_step(#{ref := Title}, Backgrounds, ReusableSteps) ->
    lookup_step(Title, Backgrounds, ReusableSteps);
lookup_step(Title, Backgrounds, ReusableSteps) when is_binary(Title) ->
    case maps:get(Title, Backgrounds, undef) of
        undef ->
            case maps:get(Title, ReusableSteps, undef) of
                undef ->
                    undef;
                Step ->
                    {Title, Step}
            end;
        Background ->
            {Title, Background}
    end;
lookup_step(Other, _, _) ->
    {error, #{error => unsupported_step_reference, ref => Other}}.

maybe_resolve_background(undef, #{title := Title} = Step) ->
    {ok, #{title => Title, steps => [Step]}};
maybe_resolve_background(undef, #{ref := Title} = Step) ->
    {ok, #{title => Title, steps => [Step]}};
maybe_resolve_background(Resolved, _) ->
    {ok, Resolved}.

enforce_resolve_background(undef, #{title := Title}) ->
    {error, #{error => missing_background_or_step, title => Title}};
enforce_resolve_background(Resolved, _) ->
    {ok, Resolved}.

resolve_steps(Steps, ReusableSteps, Procs) ->
    resolve_steps(Steps, ReusableSteps, Procs, []).

resolve_steps([], _, _, Out) ->
    {ok, lists:reverse(Out)};
resolve_steps([#{ref := Title} | Rem], ReusableSteps, Procs, Out) ->
    case resolve_step(Title, ReusableSteps, Procs) of
        {ok, S} ->
            resolve_steps(Rem, ReusableSteps, Procs, [S | Out]);
        Other ->
            Other
    end;
resolve_steps([#{type := parallel, spec := #{ref := Title}} = Spec | Rem],
              ReusableSteps,
              Procs,
              Out) ->
    case resolve_step(Title, ReusableSteps, Procs) of
        {ok, S} ->
            resolve_steps(Rem, ReusableSteps, Procs, [Spec#{spec => S} | Out]);
        Other ->
            Other
    end;
resolve_steps([#{type := fail, spec := Title} = Spec | Rem], ReusableSteps, Procs, Out)
  when is_binary(Title) ->
    case resolve_step(Title, ReusableSteps, Procs) of
        {ok, S} ->
            resolve_steps(Rem, ReusableSteps, Procs, [Spec#{spec => S} | Out]);
        Other ->
            Other
    end;
resolve_steps([#{spec := _} = Spec | Rem], ReusableSteps, Procs, Out) ->
    resolve_steps(Rem, ReusableSteps, Procs, [Spec | Out]).

resolve_step(Title, ReusableSteps, Procs) ->
    case maps:get(Title, ReusableSteps, undef) of
        undef ->
            {error, #{error => missing_step, name => Title}};
        S ->
            resolve_procedure(S, Procs)
    end.

world_with_conn(App, Props, #{conns := Conns} = World) ->
    Conns2 = Conns#{App => maps:merge(Props, #{name => App, app => App})},
    World#{conns => Conns2}.

connect([], _, World) ->
    {ok, World};
connect([S | Rem], In, World) when is_map(S) ->
    case connect(S, In, World) of
        {ok, World2} ->
            connect(Rem, In, World2);
        Other ->
            Other
    end;
connect(Spec, In, World) when is_map(Spec) ->
    case connection_with_protocol(Spec, In) of
        {ok, Spec2} ->
            case connection_with_url(Spec2, In) of
                {ok, Spec3} ->
                    case connection_with_headers(Spec3, In) of
                        {ok, Spec4} ->
                            {ok, Spec5} = connection_with_timeout(Spec4, In),
                            {ok, Spec6} = connection_with_debug(Spec5, In),
                            case connection_from(Spec6, In) of
                                {ok, #{connection := #{name := ConnName} = Conn}} ->
                                    {ok, world_with_conn(ConnName, Conn, World)};
                                {ok, Other} ->
                                    {error, #{error => not_a_connection, info => Other}};
                                Other ->
                                    Other
                            end;
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

url_spec(Spec, In) ->
    case cmencode:encode(Spec, In) of
        {ok,
         #{host := _,
           port := _,
           transport := _,
           path := _} =
             UrlSpec} ->
            {ok, UrlSpec};
        {ok, Url} when is_binary(Url) ->
            Spec2 = #{type => url, spec => Url},
            case cmencode:encode(Spec2, In) of
                {ok,
                 #{host := _,
                   port := _,
                   transport := _,
                   path := _} =
                     UrlSpec} ->
                    {ok, UrlSpec};
                Other ->
                    {error,
                     #{error => invalid_url,
                       reason => Other,
                       spec => Spec2}}
            end;
        Other ->
            {error,
             #{error => invalid_url,
               reason => Other,
               spec => Spec}}
    end.

connection_from(#{as := Name,
                  spec := #{transport := Transport} = Config0,
                  debug := Debug,
                  timeout := Timeout} =
                    Spec0,
                _) ->
    Config1 =
        Config0#{debug => Debug,
                 timeout => Timeout,
                 persistent => false},

    Config2 =
        case maps:get(protocol, Spec0, undef) of
            undef ->
                Config1;
            Protocol ->
                Config1#{protocol => Protocol}
        end,

    Config3 =
        case maps:get(headers, Spec0, undef) of
            undef ->
                Config2;
            Headers ->
                Config2#{headers => Headers}
        end,

    case cmkit:to_bin(Transport) of
        T when T =:= <<"ws">> orelse T =:= <<"wss">> ->
            Url = cmkit:url(Config3),
            {ok, Pid} = cmtest_ws_sup:new(Name, Config3, self()),
            {ok,
             #{connection =>
                   Config2#{name => Name,
                            transport => Transport,
                            class => websocket,
                            pid => Pid,
                            status => undef,
                            inbox => [],
                            url => Url}}};
        T when T =:= <<"http">> orelse T =:= <<"https">> ->
            Url = cmkit:url(Config2),
            Res = cmhttp:get(Url),
            Status =
                case Res of
                    {ok, _} ->
                        up;
                    {error, S} ->
                        S
                end,
            {ok,
             #{connection =>
                   Config3#{name => Name,
                            transport => Transport,
                            class => http,
                            status => Status,
                            inbox => [],
                            url => Url}}}
    end.

disconnect([], _, World) ->
    {ok, World};
disconnect([N | Rem], In, World) ->
    case disconnect(N, In, World) of
        {ok, World2} ->
            disconnect(Rem, In, World2);
        Other ->
            Other
    end;
disconnect(Name, _, #{conns := Conns} = World) ->
    case maps:get(Name, Conns, undef) of
        undef ->
            {error,
             #{error => not_such_connection,
               conns => maps:keys(Conns),
               info => Name}};
        #{class := websocket, pid := Pid} ->
            cmwsc:stop(Pid),
            {ok, World}
    end.

connection_with_protocol(#{protocol := #{spec := _}} = Spec, _) ->
    {ok, Spec};
connection_with_protocol(#{protocol := ProtocolSpec} = Spec, In) ->
    case cmencode:encode(ProtocolSpec, In) of
        {ok, P} ->
            {ok, Spec#{protocol => #{spec => P}}};
        Other ->
            Other
    end;
connection_with_protocol(Spec, _) ->
    {ok, Spec}.

connection_with_url(#{spec := UrlSpec} = Spec, In) ->
    case url_spec(UrlSpec, In) of
        {ok,
         #{host := _,
           port := _,
           transport := _,
           path := _} =
             Url} ->
            {ok, Spec#{spec => Url}};
        Other ->
            Other
    end.

connection_with_headers(#{headers := Headers} = Spec, In) ->
    case cmencode:encode(Headers, In) of
        {ok, H} ->
            {ok, Spec#{headers => H}};
        Other ->
            Other
    end;
connection_with_headers(Spec, _) ->
    {ok, Spec}.

connection_with_timeout(#{timeout := TimeoutSpec} = Spec, In) ->
    {ok, T} =
        case cmencode:encode(TimeoutSpec, In) of
            {ok, T0} ->
                {ok, T0};
            Other ->
                cmkit:warning({cmtest, timeout, Other, default, 1000}),
                {ok, 1000}
        end,
    {ok, Spec#{timeout => T}};
connection_with_timeout(Spec, _) ->
    {ok, Spec#{timeout => 1000}}.

connection_with_debug(#{debug := DebugSpec} = Spec, In) ->
    {ok, D} =
        case cmencode:encode(DebugSpec, In) of
            {ok, D0} ->
                {ok, D0};
            Other ->
                cmkit:warning({cmtest, debug, Other, default, false}),
                {ok, false}
        end,
    {ok, Spec#{debug => D}};
connection_with_debug(Spec, _) ->
    {ok, Spec#{debug => false}}.

probe([], _, World) ->
    {ok, World};
probe([Name | Rem], Status, World) ->
    case probe(Name, Status, World) of
        {ok, _} ->
            probe(Rem, Status, World);
        Other ->
            Other
    end;
probe(Name, Status, #{conns := Conns} = World)
  when is_atom(Name) orelse is_binary(Name) ->
    case maps:get(Name, Conns, undef) of
        undef ->
            {error, #{error => not_such_connection, info => Name}};
        #{status := Status} ->
            {ok, World};
        _ ->
            {retry, World}
    end;
probe(Other, _, _) ->
    {error, #{error => invalid_connection_name, name => Other}}.

send([], _, World) ->
    {ok, World};
send([Spec | Rem], In, World) ->
    case send(Spec, In, World) of
        {ok, World2} ->
            send(Rem, In, World2);
        Other ->
            Other
    end;
send(#{spec := #{to := Name}} = Spec, In, World) ->
    case cmencode:encode(Spec, In#{connection => Name}) of
        {ok, #{connection := #{name := Name} = Conn}} ->
            {ok, world_with_conn(Name, Conn, World)};
        {ok, Other} ->
            {error, #{error => not_a_connection, info => Other}};
        Other ->
            Other
    end.

recv([], _, World) ->
    {ok, World};
recv([S | Rem], In, World) ->
    case recv(S, In, World) of
        {ok, World2} ->
            recv(Rem, In, World2);
        Other ->
            Other
    end;
recv(#{from := Name,
       spec := Spec,
       retry := Retry} =
         RecvSpec,
     In,
     #{data := Data, conns := Conns} = World) ->
    case maps:get(Name, Conns, undef) of
        undef ->
            {error, #{error => no_such_connection, info => Name}};
        #{inbox := Inbox} ->
            Spec0 = #{type => first, spec => Spec},
            case cmdecode:decode(Spec0, Inbox, In) of
                {ok, Decoded} ->
                    case is_inverse(World) of
                        false ->
                            case maps:get(as, RecvSpec, undef) of
                                undef ->
                                    {ok, World};
                                Key when is_atom(Key) ->
                                    {ok, World#{data => maps:merge(Data, #{Key => Decoded})}};
                                RememberSpec when is_map(RememberSpec) ->
                                    case cmencode:encode(RememberSpec, Decoded) of
                                        {ok, Data2} ->
                                            {ok, World#{data => maps:merge(Data, Data2)}};
                                        {error, E} ->
                                            {error, #{error => encode_error, info => E}}
                                    end
                            end;
                        true ->
                            {error,
                             #{error => unexpected_received_message,
                               spec => Spec0,
                               connection => Name}}
                    end;
                no_match ->
                    case Retry of
                        true ->
                            {retry, World};
                        false ->
                            case is_inverse(World) of
                                false ->
                                    {error,
                                     #{error => message_not_received,
                                       spec => RecvSpec,
                                       conn => Name}};
                                true ->
                                    {ok, World}
                            end
                    end
            end
    end.

is_inverse(#{inverse := true}) ->
    true;
is_inverse(_) ->
    false.

iterate([], _, _, _, World) ->
    {ok, World};
iterate([I | Rem], As, DestSpec, Settings, #{data := Data} = World) ->
    case run(DestSpec, Settings, World#{data => Data#{As => I}}) of
        {ok, World2} ->
            iterate(Rem, As, DestSpec, Settings, World2);
        Other ->
            {error,
             #{error => unexpected_step_result,
               info => iterate,
               spec => DestSpec,
               value => Other}}
    end.

run_specs([], _, World) ->
    {ok, World};
run_specs([S | Rem] = Specs,
          Settings,
          #{retries :=
                #{wait := Wait,
                  left := Left,
                  max := Max} =
                Retries} =
              World)
  when is_map(S) ->
    case run(S, Settings, World) of
        {retry, World2} ->
            timer:sleep(Wait),
            run_specs(Specs, Settings, World2#{retries => Retries#{left => Left - 1}});
        {ok, World2} ->
            run_specs(Rem, Settings, World2#{retries => Retries#{left => Max}});
        Other ->
            Other
    end.

run(#{type := _}, _, #{retries := #{left := 0}} = World) ->
    case is_inverse(World) of
        false ->
            {error, #{error => max_retries_reached, info => none}};
        true ->
            {ok, World}
    end;
run(#{type := iterate,
      spec :=
          #{as := AsSpec,
            dest := DestSpec,
            source := SourceSpec}},
    Settings,
    World) ->
    In = World#{settings => Settings},
    case cmencode:encode(SourceSpec, In) of
        {ok, Source} ->
            case cmencode:encode(AsSpec, In) of
                {ok, As} ->
                    iterate(Source, As, DestSpec, Settings, World);
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
run(#{type := expect,
      retry := Retry,
      spec := Spec},
    Settings,
    #{data := Data} = World) ->
    In = World#{settings => Settings},
    case cmencode:encode(Spec, In) of
        {ok, false} ->
            case Retry of
                true ->
                    {retry, World};
                false ->
                    case is_inverse(World) of
                        false ->
                            {error, #{error => failed_expectation, spec => Spec}};
                        true ->
                            {ok, World}
                    end
            end;
        {ok, Encoded} ->
            case is_inverse(World) of
                true ->
                    {error,
                     #{error => unexpected_step_result,
                       info => expect,
                       spec => Spec,
                       value => Encoded}};
                false ->
                    World2 =
                        case Encoded of
                            true ->
                                World;
                            M when is_map(M) ->
                                World#{data => maps:merge(Data, M)}
                        end,
                    {ok, World2}
            end;
        Other ->
            Other
    end;
run(#{type := kube} = Spec, Settings, #{data := Data} = World) ->
    In = World#{settings => Settings},
    case cmencode:encode(Spec, In) of
        {ok, Res} ->
            Key = maps:get(as, Spec, latest),
            {ok, World#{data => Data#{Key => Res}}};
        Other ->
            Other
    end;
run(#{type := procedure,
      params := Params,
      spec := Spec} =
        Spec0,
    Settings,
    #{data := Data} = World) ->
    In0 = World#{settings => Settings},
    case cmencode:encode(Params, In0) of
        {ok, EncodedParams} ->
            In = In0#{params => EncodedParams},
            case cmencode:encode(Spec, In) of
                {ok, #{connection := #{name := Name} = Conn}} ->
                    {ok, world_with_conn(Name, Conn, World)};
                {ok, Res} ->
                    case maps:get(as, Spec0, undef) of
                        undef ->
                            {ok, World};
                        AsSpec ->
                            case cmencode:encode(AsSpec, In) of
                                {ok, As} ->
                                    Key = cmkit:to_atom(As),
                                    {ok, World#{data => Data#{Key => Res}}};
                                {error, E} ->
                                    {error,
                                     #{error => encode_error,
                                       phase => as,
                                       info => E}}
                            end
                    end;
                {error, E} ->
                    {error,
                     #{error => encode_error,
                       phase => procedure,
                       info => E}}
            end;
        {error, E} ->
            {error,
             #{error => encode_error,
               phase => params,
               info => E}}
    end;
run(#{type := connect, as := As} = Spec, Settings, World) ->
    In = World#{settings => Settings},
    case cmencode:encode(As, In) of
        {ok, Name} when is_atom(Name) orelse is_binary(Name) ->
            connect(Spec#{as => Name}, In, World);
        {ok, Names} when is_list(Names) ->
            ConnSpecs = lists:map(fun(N) -> Spec#{as => N} end, Names),
            connect(ConnSpecs, In, World);
        Other ->
            {error,
             #{error => invalid_connection_id,
               spec => Spec,
               connection_id => Other}}
    end;
run(#{type := disconnect, spec := Spec}, Settings, World) ->
    In = World#{settings => Settings},
    case cmencode:encode(Spec, In) of
        {ok, Name} when is_atom(Name) orelse is_binary(Name) ->
            disconnect(Name, In, World);
        {ok, Names} when is_list(Names) ->
            disconnect(Names, In, World);
        Other ->
            {error,
             #{error => invalid_connection_id,
               spec => Spec,
               connection_id => Other}}
    end;
run(#{type := probe, spec := #{connection := ConnSpec, status := Status}},
    Settings,
    World) ->
    In = World#{settings => Settings},
    case cmencode:encode(ConnSpec, In) of
        {ok, NameOrNames} ->
            case cmencode:encode(Status, In) of
                {ok, S} ->
                    probe(NameOrNames, cmkit:to_atom(S), World);
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
run(#{type := send, spec := #{to := Conn, spec := Spec}} = Spec0, Settings, World) ->
    In = World#{settings => Settings},
    case cmencode:encode(Conn, In) of
        {ok, Name} when is_atom(Name) orelse is_binary(Name) ->
            send(Spec0#{spec => #{to => Name, spec => Spec}}, In, World);
        {ok, Names} when is_list(Names) ->
            SendSpecs = lists:map(fun(N) -> Spec0#{spec => #{to => N, spec => Spec}} end, Names),
            send(SendSpecs, In, World);
        Other ->
            {error,
             #{error => invalid_connection_id,
               spec => Conn,
               connection_id => Other}}
    end;
run(#{type := fail, spec := Spec}, Settings, World) ->
    run(Spec, Settings, World#{inverse => true});
run(#{type := recv, from := Conn} = RecvSpec, Settings, World) ->
    In = World#{settings => Settings},
    case cmencode:encode(Conn, In) of
        {ok, Name} when is_atom(Name) orelse is_binary(Name) ->
            recv(RecvSpec#{from => Name}, In, World);
        {ok, Names} when is_list(Names) ->
            RecvSpecs = lists:map(fun(N) -> RecvSpec#{from => N} end, Names),
            recv(RecvSpecs, In, World);
        Other ->
            {error,
             #{error => invalid_connection_id,
               spec => Conn,
               connection_id => Other}}
    end;
run(#{type := merge, spec := _} = Spec, Settings, #{data := Data} = World) ->
    case cmencode:encode(Spec, #{data => Data}, Settings) of
        {ok, Data2} ->
            {ok, World#{data => Data2}};
        Other ->
            Other
    end;
run(#{type := set, spec := Spec}, Settings, #{data := Data} = World) ->
    case cmencode:encode(Spec, World#{settings => Settings}, Settings) of
        {ok, Data2} ->
            {ok, World#{data => maps:merge(Data, Data2)}};
        Other ->
            Other
    end;
run(#{type := parallel,
      count := CountSpec,
      spec := Spec0},
    Settings,
    #{data := _Data} = World) ->
    In = World#{settings => Settings},
    case cmencode:encode(CountSpec, In) of
        {ok, Count} ->
            FuncCall = {?MODULE, run, [Spec0, Settings, World]},
            FuncCalls = lists:map(fun(_) -> FuncCall end, lists:seq(1, Count)),
            _Res = rpc:parallel_eval(FuncCalls),
            {ok, World};
        Other ->
            Other
    end;
run(#{type := list, value := Specs}, Settings, World) when is_list(Specs) ->
    run_specs(Specs, Settings, World);
run(#{type := await, spec := #{http := Http, expect := Expect}} = Await,
    Settings,
    World) ->
    Debug = maps:get(debug, Await, false),
    Http2 = with_debug(Debug, Http),
    Expect2 = with_debug(Debug, Expect),

    case run(Http2, Settings, World) of
        {ok, World2} ->
            case run(Expect2, Settings, World2) of
                {ok, _} = Ok ->
                    Ok;
                _ ->
                    {retry, World2}
            end;
        _ ->
            {retry, World}
    end;
run(Spec, Settings, #{data := Data} = World) ->
    In = World#{settings => Settings},
    case encode_alias(Spec, In) of
        {ok, Alias} ->
            case cmencode:encode(Spec, In) of
                {ok, Encoded} ->
                    {ok, World#{data => Data#{Alias => Encoded}}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

encode_alias(#{as := AliasSpec}, In) ->
    case cmencode:encode(AliasSpec, In) of
        {ok, As} when is_binary(As) ->
            {ok, cmkit:to_atom(As)};
        Other ->
            Other
    end;
encode_alias(_, _) ->
    {ok, latest}.

close(#{conns := _Conns}, _Pid) ->
    ok.

report_sort_fun(#{timestamp := T1}, #{timestamp := T2}) ->
    T1 > T2.

printable(Map) ->
    cmkit:printable(128, Map).
