-module(cmeffect_bus).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> bus.

effect_apply(#{ context := Context,
                sub := T } = Spec, Pid) ->

    Res = case maps:get(create, Spec, false) of 
              false ->
                  cmbus:sub(T, Pid);
              true ->
                  cmbus:create_sub(T, Pid)
          end,

    cmcore:update(Pid, #{ topic => T,
                          context => Context,
                          status => Res });

effect_apply(#{ context := Context,
                unsub := T } = Spec, Pid) ->

    Res0 = cmbus:unsub(T, Pid),
    Res = case maps:get(delete, Spec, false) of 
              false -> 
                  Res0;
              true ->
                  cmbus:delete(T)
          end,    
    cmcore:update(Pid, #{ topic => T,
                          context => Context,
                          status => Res });

effect_apply(#{ context := Context,
                topic := Topic,
                data := Data }, Pid) ->

    case cmbus:closest(Topic) of 
        {error, E} ->
            cmcore:update(Pid, #{ context => Context,
                                  status => error,
                                  topic => Topic,
                                  error => E });
        {ok, [Pid2]} ->
            cmcore:update(Pid2, Data),
            cmcore:update(Pid, #{ context => Context,
                                  status => ok,
                                  topic => Topic })
    end.
