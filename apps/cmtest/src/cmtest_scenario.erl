-module(cmtest_scenario).
-behaviour(gen_statem).
-export([
         next/1,
         stop/1,
         start_link/6,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).

next(Pid) ->
    gen_statem:cast(Pid, next).

stop(Pid) ->
    gen_statem:call(Pid, stop).

ok(Pid) ->
    [{reply, Pid, ok}].

callback_mode() ->
    state_functions.

start_link(Test, Scenario, Steps, Facts, Settings, Runner) ->
    gen_statem:start_link(?MODULE, [Test, Scenario, Steps, Facts, Settings, Runner], []).

init([#{ config := Config }=Test, Scenario, Steps, Facts, Settings, Runner]) ->

    Retries = maps:get(retries, Config, 10),
    Wait = maps:get(wait, Config, 25),

    Data = #{ log => cmkit:log_fun(Config),
              test => Test,
              scenario => Scenario,
              steps => Steps,
              settings => Settings,
              world => #{ retries => #{ max => Retries,
                                        left => Retries,
                                        wait => Wait
                                      },
                          facts => Facts, 
                          data => #{},
                          conns => #{}},
              runner => Runner,
              started => cmkit:now()
            },

    {ok, ready, Data}.

ready(info, {ws, App, Ev}, #{ world := World }=Data) ->
    World2 = world_with_conn_status(App, Ev, World),
    {keep_state, Data#{ world => World2}};

ready(info, {ws, App, msg, Msg}, #{ world := World }=Data) ->
    World2 = world_with_conn_msg(App, Msg, World),
    {keep_state, Data#{ world => World2}};

ready(state_timeout, retry, Data) ->
    run_steps(Data);

ready(cast, next, Data) ->
    run_steps(Data);

ready({call, From}, stop, #{ 
                             world := World
                           }) ->
    cmtest_util:close(World, self()),
    {stop_and_reply, normal, ok(From)}.

terminate(_, _, _) ->
    ok.

step_started(#{ step_started := Started }) -> Started;
step_started(_) -> cmkit:now().

run_steps(#{ test := #{ name := Name },
             scenario := #{ title := Title },
             settings := Settings,
             steps := Steps,
             world := #{ retries := #{ wait := Wait,
                                       max := MaxRetries
                                     } = Retries
                       } = World,
             runner := Runner,
             started := ScenarioStarted,
             log := Log
           }=Data) ->

    case Steps of 
        [#{ title := StepTitle,
            spec := StepSpec }=Step|Rem] ->
            StepStarted = step_started(Data),
            Res = cmtest_util:run(Step, Settings, World),
            case Res of 
                {retry, World2} ->
                    Elapsed = cmkit:millis_since(ScenarioStarted),
                    World3 = retried(World2#{ elapsed => Elapsed }),
                    {keep_state, Data#{ step_started => StepStarted,
                                        world => World3 }, 
                     [{state_timeout, Wait, retry}]};

                {ok, World2} ->
                    Elapsed = cmkit:millis_since(ScenarioStarted),
                    StepMillis = cmkit:millis_since(StepStarted),
                    World3 = World2#{ inverse => false,
                                      retries => Retries#{ left => MaxRetries },
                                      elapsed => Elapsed },

                    PrintableWorld = printable_world(World3),
                    Log(PrintableWorld),
                    cmkit:success({cmtest, #{ test => Name,
                                              scenario => Title,
                                              step => StepTitle, 
                                              pid => self() }}),
                    cmtest_runner:progress(Name, Title, #{ test => Name,
                                                           scenario => Title,
                                                           step => Step,
                                                           millis => StepMillis,
                                                           world => PrintableWorld}, 
                                           length(Rem), Elapsed, Runner),
                    Data2 = Data#{ world => World3,steps => Rem },
                    Data3 = maps:without([step_started], Data2),
                    {keep_state, Data3}; 
                {error, E} ->
                    #{ data := ScenarioData, conns := ScenarioConns } = World,
                    cmkit:danger({cmtest, #{ test => Name,
                                             scenario => Title,
                                             step => StepTitle,
                                             spec => StepSpec,
                                             data => cmtest_util:printable(ScenarioData),
                                             conns => ScenarioConns,
                                             pid => self() }}),
                    StepMillis = cmkit:millis_since(StepStarted),
                    Elapsed = cmkit:millis_since(ScenarioStarted),
                    close_conns(World),
                    World2 = World#{ elapsed => Elapsed },
                    cmtest_runner:fail(Name, Title, #{ test => Name,
                                                       scenario => Title,
                                                       step => Step,
                                                       millis => StepMillis,
                                                       world => printable_world(World2),
                                                       failure => cmtest_util:printable(E)
                                                     }, Elapsed, Runner),
                    Data2 = Data#{ steps => Rem, world => World2 },
                    Data3 = maps:without([step_started], Data2),
                    {keep_state, Data3}
            end;
        [] ->
            Elapsed = cmkit:millis_since(ScenarioStarted),
            close_conns(World),
            cmkit:success({cmtest, #{ test => Name,
                                      scenario => Title,
                                      elapsed => Elapsed,
                                      pid => self() }}),
            cmtest_runner:success(Name, Title, 0, Elapsed, Runner),
            {keep_state, Data}
    end.

retried(#{ retries := #{ left := L }=R}=W) ->
    W#{ retries => R#{ left => L -1 }}.


printable_world(#{ conns := Conns } = World) ->
    cmtest_util:printable(World#{ conns => without_protocol(Conns)});

printable_world(World) -> cmtest_util:printable(World).

without_protocol(Conns) ->
    maps:fold(fun(Name, Info, Out) ->
                      Out#{ Name => maps:without([protocol], Info)} 
              end, #{}, Conns).

world_with_conn_status(App, Status, #{ conns := Conns }=World) ->
    Conn = maps:get(App, Conns),
    Conn2 = Conn#{ status => Status },
    Conns2 = Conns#{ App => Conn2 },
    World#{ conns => Conns2 }.

world_with_conn_msg(App, Msg, #{ conns := Conns}=World) ->
    Conn = maps:get(App, Conns),
    Inbox = maps:get(inbox, Conn),
    Conn2 = Conn#{ inbox => [Msg|Inbox] },
    Conns2 = Conns#{ App => Conn2 },
    World#{ conns => Conns2 }.

close_conns(#{ conns := Conns }) ->
    ConnsToClose = [ {Name, Pid} || #{ name := Name,
                                       pid := Pid,
                                       class := websocket } <- maps:values(Conns) ],
    [ close_conn(C) || C <- ConnsToClose ],
    ok;

close_conns(_) ->
    ok.

close_conn({_, Pid}) ->
    cmtest_ws_sup:stop(Pid).

