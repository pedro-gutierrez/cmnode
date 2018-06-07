-module(cmtest_scenario).
-behaviour(gen_statem).
-export([
         start/4,
         next/1,
         stop/1,
         start_link/4,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).

start(Test, Spec, Settings, Runner) ->
    cmtest_scenario_sup:start(Test, Spec, Settings, Runner).

next(Pid) ->
    gen_statem:cast(Pid, next).

stop(Pid) ->
    gen_statem:call(Pid, stop).

ok(Pid) ->
    [{reply, Pid, ok}].

callback_mode() ->
    state_functions.

start_link(Test, Spec, Settings, Runner) ->
    gen_statem:start_link(?MODULE, [Test, Spec, Settings, Runner], []).

init([#{ name := Name,
         config := #{ retries := Retries,
                      wait := Wait 
                    } 
       }=Test, #{ title := Title }=Scenario, Settings, Runner]) ->
    Log = cmkit:log_fun(true),
    case cmtest_util:steps(Scenario, Test) of 
        {ok, Steps} ->
            {ok, ready, #{ log => Log, 
                           test => Test, 
                           scenario => Scenario, 
                           steps => Steps, 
                           settings => Settings,
                           world => #{ 
                                       retries => #{ max => Retries,
                                                     left => Retries,
                                                     wait => Wait
                                                   },
                                       data => #{},
                                       conns => #{} },
                           runner => Runner
                         }};
        Other ->
            cmkit:danger({cmtest, error, Name, Title, Other}),
            {stop, normal}
    end.


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

run_steps(#{ test := #{ name := Name },
             scenario := #{ title := Title },
             settings := Settings,
             steps := Steps,
             world := #{ retries := #{ 
                                       wait := Wait 
                                     }
                       } = World,
             runner := Runner
           }=Data) ->

    case Steps of 
        [Step|Rem] ->
            Res = cmtest_util:run(Step, Settings, World),
            case Res of 
                retry ->
                    World2 = retried(World),
                    {keep_state, Data#{ world => World2  }, 
                        [{state_timeout, Wait, retry}]};
                
                {ok, World2} ->
                    cmtest_runner:progress(Name, Title, length(Rem), Runner),
                    {keep_state, Data#{ world => World2, 
                                        steps => Rem 
                                      }};
                    
                {error, E} ->
                    cmtest_runner:fail(Name, Title, #{ test => Name,
                                                       scenario => Title,
                                                       step => Step,
                                                       world => World,
                                                       reason => E
                                                     }, Runner),
                    {keep_state, Data#{ steps => Rem }}
            end;
        [] ->
            cmtest_runner:success(Name, Title, 0, Runner),
            {keep_state, Data}
    end.

retried(#{ retries := #{ left := L }=R}=W) ->
    W#{ retries => R#{ left => L -1 }}.

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
    
