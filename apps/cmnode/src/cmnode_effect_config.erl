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
    cmcore:update(SessionId, #{ buckets => cmconfig:buckets() });

effect_apply(#{ query := info, app := App}, #{ id := SessionId }) ->
    Res = case cmconfig:app(App) of 
        {ok, #{ category := Cat,
                debug := Debug,
                name := AppName
              }} ->
                  #{ app => AppName,
                     category => Cat, 
                     debug => Debug };
        _ -> 
            #{ error => not_found,
               query => info,
               app => App 
             }
    end,

    cmcore:update(SessionId, Res);


effect_apply(#{ query := modules, scope := App}, #{ id := SessionId }) ->
    Res = case cmconfig:app(App) of 
        {ok, #{ modules := Mods }} ->
                  #{ modules => Mods };
        {ok, _} ->
                  #{ modules => []};
        _ -> 
            #{ error => not_found,
               query => modules,
               app => App 
             }
    end,

    cmcore:update(SessionId, Res).
