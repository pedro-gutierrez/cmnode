-module(cmnode_effect_now).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> now.
effect_apply(#{ context := _ } = Query, #{ id := Id }) ->
    cmcore:update(Id, Query#{ now => cmkit:now() }).
