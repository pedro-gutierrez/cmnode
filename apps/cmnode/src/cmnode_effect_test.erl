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
                         scenarios => lists:map(fun(S) ->
                                                       maps:with([title, tags, purpose], S) 
                                                end, Scenarios),
                         backgrounds => lists:map(fun(B) ->

                                                       maps:with([title], B) 
                                                  end, maps:values(Backgrounds)) }
          end,

    cmcore:update(SessionId, #{ test => Res});


effect_apply(#{ query := scenario,
                test := Test,
                scenario := Scenario}, SessionId) ->
    Res = case cmconfig:scenario(Test, Scenario) of 
              {error, E} ->
                  #{ test => Test,
                     title => Scenario,
                     error => E};
              {ok, S} -> S
          end,

    cmcore:update(SessionId, #{ scenario => Res}).
