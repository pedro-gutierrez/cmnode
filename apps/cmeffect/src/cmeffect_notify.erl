-module(cmeffect_notify).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> notify.
effect_apply(Data, Id) ->
    cmcore:notify(Id, Data).
