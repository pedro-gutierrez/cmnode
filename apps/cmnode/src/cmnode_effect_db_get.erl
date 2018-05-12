-module(cmnode_effect_db_get).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_get.
effect_apply(#{ bucket := Db, 
                type := Type, 
                id := Id } = Q, #{ id := SessionId }) ->

    cmcore:update(SessionId, case cmdb:get(Db, {Type, Id}) of
                                 not_found -> Q#{ error => not_found };
                                 {ok, [V]} -> Q#{ value => V };
                                 {ok, [V|_]} -> Q#{ value => V };
                                 {error, E }-> Q#{ error => E }
                             end);

effect_apply(#{  bucket := Db, 
                 type := Type } = Q, #{ id := SessionId }) ->
    cmcore:update(SessionId, case cmdb:find(Db, Type) of
                                 {ok, Values} -> Q#{ values => Values };
                                 {error, E }-> Q#{ error => E }
                             end).
