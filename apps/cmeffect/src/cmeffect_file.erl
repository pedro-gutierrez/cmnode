-module(cmeffect_file).
-export([ effect_info/0,
          effect_apply/2,
          effect_stream/1
        ]).

effect_info() -> file.

effect_apply(#{ stream := Stream,
                bytes := Bytes,
                asset := Asset } = Q, SessionId) ->
    cmfs:stream(Q#{ bytes => Bytes, 
                    path => cmkit:asset(Asset),
                    context => #{ data => #{ id => SessionId,
                                             stream => Stream },
                                  callback => { ?MODULE, effect_stream } }}).

effect_stream(#{ stream := Stream,
                 event := Ev,
                 id := SessionId, 
                 data := Data }=Info) -> 
    cmkit:log({cmeffect, file, Info}),
    cmcore:update(SessionId, #{ stream => Stream,
                                event => Ev,
                                data => Data }). 
