-module(cmnode_effect_config).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> config.
effect_apply(#{ query := apps }, #{ id := SessionId }) ->
    cmcore:update(SessionId, #{ apps => cmconfig:apps() });

effect_apply(#{ query := ports }, #{ id := SessionId }) ->
    cmcore:update(SessionId, #{ ports => cmconfig:ports() });

effect_apply(#{ query := buckets}, #{ id := SessionId }) ->
    cmcore:update(SessionId, #{ buckets => cmconfig:buckets() }).
