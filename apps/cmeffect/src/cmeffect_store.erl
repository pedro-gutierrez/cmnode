-module(cmeffect_store).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> store.
effect_apply(_, _) -> ok.
