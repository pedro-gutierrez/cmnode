-module(cmeffect_elementary).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> elementary.

effect_apply(#{ settings := SettingsNames,
                app := Spec}, Id) ->

    Res = case resolve_settings(SettingsNames) of 
              {ok, Settings } -> 
                  compile(Spec, Settings);
              {error, E} -> 
                  #{ status => error,
                     reason => E }

          end,
    cmcore:update(Id, Res);

effect_apply(#{ app := Spec}, Id) ->
    cmcore:update(Id, compile(Spec, #{})).


resolve_settings(Name) when is_binary(Name) or is_atom(Name) ->
    case cmconfig:settings(Name) of 
        {ok, #{ spec := Spec}} ->
            cmencode:encode(Spec); 
        {error, E} ->
            {error, #{ settings => Name,
                       reason => E }}
    end;


resolve_settings(Names) when is_list(Names) ->
    resolve_settings(Names, #{}).

resolve_settings([], Spec) -> {ok, Spec};
resolve_settings([N|Rem], Spec) ->
    case resolve_settings(N) of 
        {ok, Spec0} ->
            resolve_settings(Rem, maps:merge(Spec, Spec0));
        Other ->
            Other
    end.

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

