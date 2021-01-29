-module(cmeffect_jwt).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> jwt.
effect_apply(#{ context := C,
                sign := P,
                key := K,
                ttl := T}, Id) ->

    R = case cmjwt:sign(P, T, K) of 
            error ->
                #{ context => C,
                   status => error };
            Signed -> 
                #{ context => C,
                   status => ok,
                   jwt => Signed }
        end,
    cmcore:update(Id, R );

effect_apply(#{ context := C,
                verify := P,
                key := K }, Id) ->

    R = case cmjwt:verify(P, K) of 
            false ->
                #{ context => C,
                   status => error };
            expired ->
                #{ context => C,
                   status => expired };
            JWT -> 
                #{ context => C,
                   status => ok,
                   jwt => JWT }
        end,
    cmcore:update(Id, R).

