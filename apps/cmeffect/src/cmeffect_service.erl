-module(cmeffect_service).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> service.
effect_apply(#{ service := Service,
                params := Params}, Pid) ->

    case cmservice:run(Service, Params) of 
        {error, E} ->
            cmcore:update(Pid, #{ service => Service,
                                  status => error,
                                  reason => E });
        ok ->
            cmcore:update(Pid, #{ service => Service,
                                  status => ok })
    end.
