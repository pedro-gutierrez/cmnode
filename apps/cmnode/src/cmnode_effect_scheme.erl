-module(cmnode_effect_scheme).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> scheme.


effect_apply(#{ settings := SettingsName,
                app := Spec}, Id) ->
   
    Res = case cmconfig:settings(SettingsName) of 
        {ok, #{ spec := SettingsSpec}} -> 
            case cmencode:encode(SettingsSpec) of 
                {ok, Settings} -> 
                    compile_render(Spec, Settings);
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

effect_apply(#{ app := Spec}, Id) ->
    cmcore:update(Id, compile_render(Spec, #{})).

compile_render(Spec, Settings) -> 
    case cmscheme:compile(Spec, Settings) of 
        {ok, Ast} ->
            case cmscheme:render(Ast) of 
                {ok, Source} ->
                    #{ language => scheme,
                       status => ok,
                       source => Source };
                {error, Error} ->
                    #{ language  => scheme,
                       status => error,
                       reason => Error
                     }
            end;
        {error, Error} -> 
            #{ language  => scheme,
               status => error,
               reason => Error
             }
    end.

