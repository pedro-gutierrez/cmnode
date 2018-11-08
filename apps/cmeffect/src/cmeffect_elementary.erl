-module(cmeffect_elementary).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> elementary.


effect_apply(#{ settings := SettingsName,
                app := Spec}, Id) ->
   
    Res = case cmconfig:settings(SettingsName) of 
        {ok, #{ spec := SettingsSpec}} -> 
            case cmencode:encode(SettingsSpec) of 
                {ok, Settings} -> 
                    compile(Spec, Settings);
                {error, E} -> 
                    #{ status => error,
                       reason => E }
            end;
        {error, E} -> 
                    #{ status => error,
                       reason => E,
                       settings => SettingsName }

    end,
    cmcore:update(Id, Res);

effect_apply(#{ app := Spec}, Id) ->
    cmcore:update(Id, compile(Spec, #{})).

compile(Spec, Settings) -> 
    case cmelementary:compile(Spec, Settings) of 
        {ok, Source} ->
            #{ language => elementary,
               status => ok,
               source => Source };
        {error, Error} -> 
            #{ language  => elementary,
               status => error,
               reason => Error
             }
    end.

