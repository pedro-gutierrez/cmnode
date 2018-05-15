-module(cmnode_effect_db_get).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_get.

effect_apply(#{ bucket := Db, 
                type := Type, 
                id := Id } = Q, #{ id := SessionId }) ->

    R = case cmdb:get(Db, {Type, Id}) of 
            not_found -> Q#{ error => not_found };
            {ok, Items} -> Q#{ value => value(Items, maps:get(all, Q, false))};
            {error, E }-> Q#{ error => E }
        end,
    cmcore:update(SessionId, R);

effect_apply(#{  bucket := Db, 
                 type := Type } = Q, #{ id := SessionId }) ->
    cmcore:update(SessionId, case cmdb:find(Db, Type) of
                                 {ok, Values} -> Q#{ values => Values };
                                 {error, E }-> Q#{ error => E }
                             end).

value([V], false) -> V;
value([V|_], false) -> V;
value(Items, _) -> Items.
