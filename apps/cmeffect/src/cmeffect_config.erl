-module(cmeffect_config).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> config.
effect_apply(#{ query := apps }, SessionId) ->
    cmcore:update(SessionId, #{ apps => cmconfig:apps() });

effect_apply(#{ query := ports }, SessionId) ->
    cmcore:update(SessionId, #{ ports => cmconfig:ports() });

effect_apply(#{ query := buckets}, SessionId) ->
    cmcore:update(SessionId, #{ buckets => cmconfig:buckets() });

effect_apply(#{ query := info, app := App}, SessionId) ->
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

effect_apply(#{ query := config, app := App}, SessionId) ->
    Res = case cmconfig:app(App) of 
              {ok, #{ config := Config }} ->
                  #{ config => Config };
              _ -> 
                  #{ error => not_found,
                     query => config,
                     app => App 
                   }
          end,

    cmcore:update(SessionId, Res);


effect_apply(#{ query := modules, scope := App}, SessionId) ->
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

    cmcore:update(SessionId, Res);

effect_apply(#{ query := module, module := Mod}, SessionId) ->
    Res = case cmconfig:module(Mod) of 
              {ok, Spec} ->
                  #{ module => Spec};
              _ -> 
                  #{ error => not_found,
                     query => modules,
                     module => Mod 
                   }
          end,
    cmcore:update(SessionId, Res);

effect_apply(#{ query := settings, settings := Name}, SessionId) ->
    Res = case cmconfig:settings(Name) of 
              {ok, Spec} -> 
                  #{ settings => Spec };
              {error, E} -> 
                  #{ error => E,
                     query => settings,
                     settings => Name
                   }
          end,

    cmcore:update(SessionId, Res);

effect_apply(#{ query := settings}, SessionId) ->
    cmcore:update(SessionId, #{ settings => cmconfig:settings() }).

