-module(cmnode_effect_css).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> css.


effect_apply(#{ settings := SettingsName,
                styles := Spec}, Id) ->
   
    Res = case cmconfig:settings(SettingsName) of 
        {ok, #{ spec := SettingsSpec}} -> 
            case cmencode:encode(SettingsSpec) of 
                {ok, Settings} -> 
                    compile(Spec, Settings);
                {error, E} -> 
                    #{ status => error,
                       reason => encode,
                       reason => E }
            end;
        {error, E} -> 
                    #{ status => error,
                       reason => E,
                       settings => SettingsName }

    end,
    cmcore:update(Id, Res);

effect_apply(#{ styles := Spec}, Id) ->
    cmcore:update(Id, compile(Spec, #{})).

compile(Spec, Settings) -> 
    case cmcss:compile(Spec, Settings) of 
        {ok, Source} ->
            #{ language => css,
               status => ok,
               source => Source };
        {error, Error} -> 
            #{ language  => css,
               status => error,
               reason => Error
             }
    end.

