-module(cmeffect_db_reset).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_reset.


effect_apply(#{ bucket := B } = Q, SessionId) ->
    cmcore:update(SessionId, Q#{ status => cmdb:reset(B) });

effect_apply(#{ buckets := Buckets } = Q, SessionId) ->
    Requested = [ cmdb:reset(B) || B <- Buckets],
    Deleted = lists:filter(fun(ok) -> true; 
                                (_) -> false end, Requested),
    Status = case length(Requested) =:= length(Deleted) of 
                 true -> ok;
                 false -> partial
             end,
    cmcore:update(SessionId, Q#{ status => Status }).
