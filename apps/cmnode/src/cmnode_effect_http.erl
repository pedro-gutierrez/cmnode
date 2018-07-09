-module(cmnode_effect_http).
-export([ effect_info/0,
          effect_apply/2,
          effect_stream/1
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
    
    cmcore:update(SessionId, Data#{ context => Context});

effect_apply(#{ stream := Stream } = Q, SessionId) ->
    cmkit:log({cmeffect, http, Q}),
    cmhttp:stream(Q#{ context => #{ data => #{ id => SessionId, 
                                               stream => Stream },
                                    callback => { ?MODULE, effect_stream } }}).

effect_stream(#{ stream := Stream,
                 event := Ev,
                 id := SessionId, 
                 data := Data }=Info) -> 
    cmkit:log({cmeffect, http, Info}),
    cmcore:update(SessionId, #{ stream => Stream,
                                event => Ev,
                                data => Data }). 
