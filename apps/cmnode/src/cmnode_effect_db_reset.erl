-module(cmnode_effect_db_reset).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_reset.
effect_apply(#{ buckets := Buckets }, #{ id := SessionId }) ->
    Deleted = [ cmdb:reset(B) || B <- Buckets],
    DeletedOk = lists:filter(fun(ok) -> true; 
                                (_) -> false end, Deleted),
    Res = #{ all_deleted => length(DeletedOk) =:= length(Deleted)},
    cmkit:log({db_reset, Buckets, Res}), 
    cmcore:update(SessionId, Res).
