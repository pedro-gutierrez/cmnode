-module(cmnode_effect_db_get).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_get.
effect_apply(#{ context := Context, 
                bucket := Db, 
                type := Type, 
                id := Id }, #{ id := SessionId }) ->
    Res = #{ context => Context,
             bucket => Db,
             type => Type,
             id => Id },
    cmcore:update(SessionId, case cmdb:get(Db, {Type, Id}) of
                                 not_found -> Res#{ error => not_found };
                                 {ok, [V]} -> Res#{ value => V };
                                 {ok, [V|_]} -> Res#{ value => V };
                                 {error, E }-> Res#{ error => E }
                             end).
