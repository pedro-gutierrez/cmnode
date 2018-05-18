-module(cmtest_util).
-export([start/3,
         scenarios/2,
         steps/2,
         run/2,
         close/2]).

start(Test, #{ title := Title}=Scenario, Runner) ->
    case cmtest_scenario:start(Test, Scenario, Runner) of 
        {ok, Pid} ->
            {ok, Pid};
        Other -> 
            {error, {Title, Other}}
    end.

scenarios(Token, Scenarios) ->
    {ok, lists:filter(fun(#{ tags := Tags}) ->
                              lists:member(Token, Tags)
                      end, Scenarios)}.

steps(#{ steps := Steps,
         backgrounds := Backgrounds }, Test) ->
    case resolve_backgrounds(Backgrounds, Test) of 
        {ok, Resolved } ->
            {ok, lists:flatten(lists:map(fun(#{ steps := Steps2 }) -> 
                                            Steps2 
                                    end, Resolved)) ++ Steps};
        Other -> Other
    end.

resolve_backgrounds(Backgrounds, Test) ->
    resolve_backgrounds(Backgrounds, Test, []).

resolve_backgrounds([], _, Out) -> {ok, lists:reverse(Out)};
resolve_backgrounds([B|Rem], #{ backgrounds := Backgrounds }=Test, Out) ->
    case maps:get(B, Backgrounds, undef) of 
        undef -> {error, {undefined, background, B}};
        Resolved -> resolve_backgrounds(Rem, Test, [Resolved|Out])
    end.

world_with_new_conn(App, Pid, #{ conns := Conns }=World) ->
    Conns2 = Conns#{ App => #{ pid => Pid,
                               status => unknown,
                               inbox => [],
                               name => App,
                               app => App
                             }},
    World#{ conns => Conns2 }.

run(#{ type := connection,
       port := Port,
       app := App, 
       transport := Transport }, #{ data := _Data
                                  }=World) ->
    
    case cmconfig:mount(App, Port, Transport) of 
        {error, not_found} ->
            {error, {not_found, App, Port, Transport}};
        {ok, Mount } ->
            Config = Mount#{ debug => false ,
                             host => "localhost",
                             persistent => false 
                           },
            {ok, Pid } = cmtest_ws_sup:new(App, Config, self()),
            
            World2 = world_with_new_conn(App, Pid, World), 
            {ok, World2 }
    end;

run(#{ type := probe, 
       spec := #{ app := App,
                  status := Status 
                }}, #{ conns := Conns, retries := R }=World) ->
    case maps:get(App, Conns, undef) of
        undef ->
            {error, {not_found, App, World}};
        #{ status := Status } ->
            {ok, World};
        _ ->
            {retry, World#{ retries => R -1}}
    end;

run(#{ type := expect, 
       spec := Spec } , #{ retries := 0}=World) ->
    {error, {timeout, Spec, World}};

run(#{ type := expect, 
       spec := Spec } , #{ retries := R}=World) ->
    case cmeval:eval(Spec, World) of 
        true ->
            {ok, World};
        false ->
            {retry, World#{ retries => R -1}}
    end;

run(#{ type := send, 
       spec := #{ 
         to := App,
         spec := Spec 
        }
     } , #{ conns := Conns }=World) ->
    case maps:get(App, Conns, undef) of 
        undef -> 
            {error, {not_found, App, Conns}};
        #{ pid := Pid } = Conn ->
            case cmencode:encode(Spec, World) of 
                {ok, Encoded} ->
                    cmwsc:send(Pid, Encoded),
                    {ok, World#{ conns => Conns#{ 
                                            App => Conn#{inbox => []}}}};
                Other -> Other
            end
    end;

run(#{ type := recv, 
       spec := Spec } , #{ retries := 0}=World) ->
    {error, {timeout, Spec, World}};

run(#{ type := recv, 
       spec := #{
         as := As,
         from := App,
         spec := Spec
        }} , #{ data := Data,
                conns := Conns,
                retries := R 
              }=World) ->
    case maps:get(App, Conns, undef) of 
        undef -> 
            {error, {not_found, App, Conns}};
        #{ inbox := Inbox } ->
            case cmdecode:decode(#{ type => first,
                                    spec => Spec }, Inbox) of 
                {ok, Decoded} ->
                    {ok, World#{ data => Data#{ As => Decoded }}};
                no_match -> 
                    {retry, World#{ retries => R - 1}}
            end
    end;


run(Step, _) ->
    {error, {unsupported, Step}}.

close(#{ conns := Conns }, _Pid) ->
    lists:foreach(fun(#{ name := Name,
                         pid := Pid }) ->
                          cmkit:log({cmtest, closing, Name, Pid})
    %                      cmtest_ws_sup:stop(Pid)
                  end, maps:values(Conns)),
    ok.
