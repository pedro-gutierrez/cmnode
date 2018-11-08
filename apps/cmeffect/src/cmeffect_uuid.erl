-module(cmeffect_uuid).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> uuid.
effect_apply(#{ context := _ } = Query, Id) ->
    cmcore:update(Id, Query#{ uuid => cmkit:uuid() }).
