-module(cmnode_effect_db_put_all).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db_put_all.
effect_apply(#{ context := Context, 
                bucket := Db, 
                items := Items }, #{ id := SessionId }) ->
    Pairs = lists:map( fun(#{ id := Id,
                              type := Type,
                              value := Value}) -> {{Type, Id}, Value} end, Items),
    
    Res = #{ context => Context,
             bucket => Db,
             status => cmdb:put(Db, Pairs) },
    cmcore:update(SessionId, Res).
