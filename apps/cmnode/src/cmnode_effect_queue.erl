-module(cmnode_effect_queue).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> queue.

effect_apply(#{ query := queues }, SessionId) ->
    Res = [ with_status(Q) || Q <- cmconfig:queues() ],
    cmcore:update(SessionId, #{ queues => Res});

effect_apply(#{ query := queue,
                queue := Name }, SessionId) ->
    Res = case cmconfig:queue(Name) of 
        {ok, Spec} ->
                  with_status(Spec);
        Other -> Other
    end,

    cmcore:update(SessionId, #{ queue => Res}).

with_status(#{ worker := Name }=Spec) ->
    case cmqueue:status(Name) of 
        {ok, Status } -> 
            Spec#{ status => Status };
        {error, E} ->
            Spec#{ error => E }
    end.
    
