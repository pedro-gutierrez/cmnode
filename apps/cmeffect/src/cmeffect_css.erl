-module(cmeffect_css).
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
                              {ok, Colors, Fonts, FontSizes, Selectors} -> 
                                  case cmcss:compile(Colors, Fonts, FontSizes, Selectors, Settings) of 
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
                          err(ThemeNames, #{ status => E,
                                             settings => SettingsName })
                  end;
              {error, E} -> 
                  err(ThemeNames, #{ status => E,
                                     settings => SettingsName }) 
          end,
    cmcore:update(Id, Res).

err(Themes, I) -> 
    cmkit:danger({cmeffect_css, Themes, I}),
    #{ language  => css,
       status => error,
       reason => Themes
     }.

resolve(Themes) -> resolve(Themes, [], [], [], []).
resolve([], C, F, FS, S) -> {ok, lists:reverse(C), lists:reverse(F), lists:reverse(FS), lists:reverse(S)};
resolve([N|Rem], C, F, FS, S) -> 
    case cmconfig:theme(N) of 
        {ok, #{ spec := #{ colors := C0, 
                           selectors := S0,
                           fonts := F0,
                           font_sizes := FS0
                         } }} -> 
            resolve(Rem, [C0|C], [F0|F], [FS0|FS], [S0|S]);
        Other -> 
            Other
    end.
