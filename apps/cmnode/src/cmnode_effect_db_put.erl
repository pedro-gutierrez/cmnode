-module(cmnode_effect_db_put).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_put.

effect_apply(#{ context := Context, 
                bucket := Db, 
                items := Items }=Spec, SessionId) ->
    
    Strategy = strategy(Spec),
    Pairs = pairs(Items),
    Res = #{ context => Context,
             bucket => Db,
             status => cmdb:Strategy(Db, Pairs) },

    cmcore:update(SessionId, Res);

effect_apply(#{ context := Context,
                bucket := Db, 
                type := Type,
                id := Id,
                value := Value }=Spec, SessionId) ->
    
    Strategy = strategy(Spec),
    Pairs = [{{Type, Id}, Value}],
    Res = #{ context => Context,
             bucket => Db,
             type => Type,
             status => cmdb:Strategy(Db, Pairs) }, 
    Res2 = return(Spec, Res),
    cmcore:update(SessionId, Res2).

strategy(#{ new := true }) -> put_new;
strategy(_) -> put.

pairs(Items) when is_list(Items) ->
    lists:map( fun(#{ id := Id,
                      type := Type,
                      value := Value}) -> {{Type, Id}, Value} end, Items).

return(#{ echo := true,
          value := Value }, Res ) -> 
    Res#{ value => Value };

return(_, Res ) -> Res.
