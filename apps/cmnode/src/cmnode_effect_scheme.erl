-module(cmnode_effect_scheme).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> scheme.

effect_apply(#{ app := Spec}, #{ id := Id }) ->
    Res = case cmscheme:compile(Spec) of 
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
          end,
    cmcore:update(Id, Res).
