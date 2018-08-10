-module(cmnode_effect_css).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> css.


effect_apply(#{ settings := SettingsName,
                themes := ThemeNames}, Id) ->
   
    Res = case cmconfig:settings(SettingsName) of 
        {ok, #{ spec := SettingsSpec}} -> 
            case cmencode:encode(SettingsSpec) of 
                {ok, Settings} ->
                    case resolve(ThemeNames) of 
                        {ok, Colors, Selectors} -> 
                            case cmcss:compile(Colors, Selectors, Settings) of 
                                {ok, Source} ->
                                    #{ language => css,
                                       status => ok,
                                       source => Source };
                                {error, Error} -> 
                                    err(ThemeNames, Error)
                            end;
                        {error, E} -> 
                            err(ThemeNames, E)
                    end;
                {error, E} -> 
                    err(ThemeNames, E)
            end;
        {error, E} -> 
                  err(ThemeNames, E) 
    end,
    cmcore:update(Id, Res).

err(Themes, I) -> 
    cmkit:danger({cmnode_effect_css, Themes, I}),
    #{ language  => css,
       status => error,
       reason => Themes
     }.

resolve(Themes) -> resolve(Themes, [], []).
resolve([], C, S) -> {ok, lists:reverse(C), lists:reverse(S)};
resolve([N|Rem], C, S) -> 
    case cmconfig:theme(N) of 
        {ok, #{ spec := #{ colors := C0, 
                           selectors := S0 } }} -> 
            resolve(Rem, [C0|C], [S0|S]);
        Other -> 
            Other
    end.
