-module(cmeffect_html).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> html.


effect_apply(#{ settings := SettingsName,
                page := Page,
                app := #{ views := Views}}, Id) ->

    Res = case cmconfig:settings(SettingsName) of 
              {ok, #{ spec := SettingsSpec}} -> 
                  case cmencode:encode(SettingsSpec) of 
                      {ok, Settings} -> 
                          compile(Page, Views, Settings);
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

effect_apply(Page, Id) ->
    cmcore:update(Id, compile(Page, #{}, #{})).


compile(Page, Views, Settings) -> 
    case cmhtml:compile(Page, Views, Settings) of 
        {ok, Source} ->
            #{ language => html,
               status => ok,
               source => Source };
        {error, Error} -> 
            #{ language  => html,
               status => error,
               reason => Error
             }
    end.

