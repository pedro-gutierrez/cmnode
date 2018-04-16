-module(cmnode_effect_uuid).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> uuid.
effect_apply(_, #{ id := Id }) ->
    cmcore:update(Id, #{ uuid => cmkit:uuid() }).
