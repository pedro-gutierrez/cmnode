-module(cmeffect_bus).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> bus.

effect_apply(#{ sub := [Prefix, Value] = T,
                create := true }, Pid) ->
    
    Topic = {Prefix, Value},
    Res = cmbus:create_sub(Topic, Pid),
    cmcore:update(Pid, #{ topic => T,
                          status => Res });

effect_apply(#{ context := Context,
                pub := Topics,
                data := Data }, Pid) ->
    
    Payload = #{ data => Data },
    lists:foreach(fun([P, V]) ->
                    cmbus:pub({P, V}, Payload)      
                  end, Topics),

    cmcore:update(Pid, #{ context => Context,
                          status => ok });

effect_apply(#{ subscription := Topic }, Pid) ->
    Res = case cmconfig:topic(Topic) of 
              {ok, #{ name := T}} ->
                  cmbus:sub(T, Pid);
              {error, E} ->
                  E
          end,
    cmcore:update(Pid, #{ topic => Topic,
                          subscription => Res }).
