-module(cmnode_effect_db_put).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_put.

effect_apply(#{ context := Context, 
                bucket := Db, 
                new := true,
                items := Items }, #{ id := SessionId }) ->
    
    Pairs = pairs(Items),
    cmcore:update(SessionId, #{ context => Context,
                                bucket => Db,
                                status => cmdb:put_new(Db, Pairs) });

effect_apply(#{ context := Context, 
                bucket := Db, 
                items := Items }, #{ id := SessionId }) ->
    
    Pairs = pairs(Items),
    cmcore:update(SessionId, #{ context => Context,
                                bucket => Db,
                                status => cmdb:put(Db, Pairs) });


effect_apply(#{ context := Context, 
                bucket := Db, 
                type := Type,
                id := Id,
                new := true,
                value := Value }, #{ id := SessionId }) ->
    
    Pairs = [{{Type, Id}, Value}],
    cmcore:update(SessionId, #{ context => Context,
                                bucket => Db,
                                status => cmdb:put_new(Db, Pairs) });


effect_apply(#{ context := Context, 
                bucket := Db, 
                type := Type,
                id := Id,
                value := Value }, #{ id := SessionId }) ->

    Pairs = [{{Type, Id}, Value}],
    cmcore:update(SessionId, #{ context => Context,
                                bucket => Db,
                                status => cmdb:put(Db, Pairs) }).


pairs(Items) when is_list(Items) ->
    lists:map( fun(#{ id := Id,
                      type := Type,
                      value := Value}) -> {{Type, Id}, Value} end, Items).

