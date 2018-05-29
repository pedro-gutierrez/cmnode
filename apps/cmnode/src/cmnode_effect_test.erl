-module(cmnode_effect_test).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> test.

effect_apply(#{ query := tests }, SessionId) ->
    Res = [ #{ name => Name,
               scenarios => length(Scenarios),
               backgrounds => map_size(Backgrounds) 
             } || #{ name := Name, 
                  backgrounds := Backgrounds, 
                  scenarios := Scenarios 
                } <- cmconfig:tests() ],
    
    
    cmcore:update(SessionId, #{ tests => Res});

effect_apply(#{ query := test,
                test := Name }, SessionId) ->
    Res = case cmconfig:test(Name) of 
              {error, E} ->
                  #{ test => Name,
                     error => E};
              {ok, #{ backgrounds := Backgrounds,
                      scenarios := Scenarios,
                      name := Id, 
                      config := Config }} -> 

                      #{ name => Id,
                         config => cmkit:to_list(Config, property, value),
                         scenarios => lists:map(fun metadata/1, Scenarios),
                         backgrounds => lists:map(fun metadata/1, maps:values(Backgrounds)) }

          end,

    cmcore:update(SessionId, #{ test => Res});


effect_apply(#{ query := scenario,
                test := Test,
                scenario := Scenario}, SessionId) ->
    
    Res = case cmconfig:test(Test) of 
              {error, E } ->
                  #{ test => Test,
                     title => Scenario,
                     error => E};
              {ok, #{ scenarios := Scenarios }} ->
                  case cmkit:find_by(title, Scenario, Scenarios) of 
                      not_found ->
                          #{ test => Test,
                             title => Scenario,
                             error => not_found };
                      {ok, Spec} ->
                          Spec#{ test => Test}
                  end
          end,
    
    cmcore:update(SessionId, #{ scenario => Res});

effect_apply(#{ query := background,
                test := Test,
                background := Background }, SessionId) when is_binary(Background) ->
    
    Res = case cmconfig:test(Test) of 
              {error, E } ->
                  #{ test => Test,
                     title => Background,
                     error => E};
              {ok, #{ backgrounds := Backgrounds }} ->
                  case cmkit:find_by(title, Background, maps:values(Backgrounds))  of 
                      not_found ->
                          #{ test => Test,
                             title => Background,
                             error => not_found };
                      {ok, Spec} ->
                          Spec#{ test => Test}
                  end
          end,

    cmcore:update(SessionId, #{ background => Res});


effect_apply(#{ query := schedule,
                test := Test }, SessionId) ->

    Res = case cmtest:schedule(Test) of 
              {ok, Status } -> Status;
              {error, E} -> E
          end,
    cmcore:update(SessionId, #{ tests => Res});

effect_apply(#{ query := clear_queue }, SessionId) ->

    Res = case cmtest:clear() of 
              {ok, Status } -> Status;
              {error, E} -> E
          end,
    cmcore:update(SessionId, #{ tests => Res});

effect_apply(#{ query := reports,
                days := Days }, SessionId) ->

    Res = case cmtest:reports(Days) of 
              {ok, Reports } -> Reports;
              {error, E} -> E
          end,
    cmcore:update(SessionId, #{ test_reports => Res}).

metadata(Spec) ->
    maps:with([title, id, tags], Spec).
