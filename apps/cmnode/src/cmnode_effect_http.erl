-module(cmnode_effect_http).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> http.

effect_apply(#{ method := get, 
                url := Url, 
                context := Context } = Q, SessionId) ->

    
    cmkit:log({cmeffect, http, out, Q}),
    Data = case cmhttp:get(Url) of 
               {ok, In} -> 
                   In;
               {error, E} -> 
                   #{ error => E }
           end,
    
    cmcore:update(SessionId, Data#{ context => Context}).
