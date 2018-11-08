-module(cmeffect_stream).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> stream.
effect_apply(#{ event := Ev, 
                data := Data }, Id) ->
    cmcore:stream(Id, {Ev, Data}).
