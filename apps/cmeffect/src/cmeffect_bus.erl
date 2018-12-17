-module(cmeffect_bus).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> bus.
effect_apply(#{ subscription := Topic }, Pid) ->
    Res = case cmconfig:topic(Topic) of 
              {ok, #{ name := T}} ->
                  cmbus:sub(T, Pid);
              {error, E} ->
                  E
          end,
    cmcore:update(Pid, #{ topic => Topic,
                          subscription => Res }).
