-module(cmeffect_terminate).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> terminate.

effect_apply(Data, Id) ->
    cmcore:terminate(Id, Data).
