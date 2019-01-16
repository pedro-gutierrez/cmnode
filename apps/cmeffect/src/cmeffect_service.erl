-module(cmeffect_service).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> service.
effect_apply(#{ service := Service,
                params := Params}, Pid) ->

    case cmservice:run(Service, Pid, Params) of 
        {ok, _} -> 
            ok;
        {error, E} ->
            cmcore:update(Pid, #{ service => Service,
                                  status => error,
                                  reason => E })
    end.
