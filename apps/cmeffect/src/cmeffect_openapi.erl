-module(cmeffect_openapi).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> openapi.

effect_apply(#{ port := Port,
                api_title := ApiTitle,
                apps := AppNames }, Id) ->


    Res = case cmconfig:port(Port) of
              {ok, #{ apps := Apps }} ->
                  case resolve_apps(Apps, AppNames, #{}) of 
                      {ok, ResolvedApps} ->
                          case cmopenapi:spec(ResolvedApps) of 
                              {ok, Spec} ->
                                  Info = #{ title => ApiTitle,
                                            version => <<"1.0.0">> },

                                  Spec#{ openapi => <<"3.0.0">>,
                                         info => Info };
                              Other ->
                                  Other
                          end;
                      Other ->
                          Other
                  end;
              Other ->
                  Other
          end,

    Res2 = case Res of 
               {error, E} ->
                   #{ status => error,
                      reason => E };
               Data ->
                   #{ status => ok,
                      data => Data }
           end,

    cmcore:update(Id, Res2).


resolve_apps([], _, Out) -> {ok, Out};
resolve_apps([#{ name := App,
                 mounts := Mounts }|Rem], AppNames, Out) ->
    case lists:member(App, AppNames) of 
        true ->
            case http_mount(Mounts) of 
                none ->
                    {error, #{ app => App,
                               http => missing }};
                Path ->
                    case cmconfig:app(App) of 
                        {ok, AppSpec} ->
                            resolve_apps(Rem, AppNames, Out#{ App => #{ spec => AppSpec,
                                                                        path => Path }});
                        {error, E} -> 
                            {error, #{ app => App,
                                       error => E}}
                    end
            end;
        false ->
            resolve_apps(Rem, AppNames, Out)
    end.


http_mount([]) -> none;
http_mount([#{ transport := http,
               path := Path }|_]) -> Path;
http_mount([_|Rem]) -> http_mount(Rem).

