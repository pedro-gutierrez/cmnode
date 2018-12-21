-module(cmeffect_terminate).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> terminate.

effect_apply(_, SessionId) ->
    cmcore:terminate(SessionId).
