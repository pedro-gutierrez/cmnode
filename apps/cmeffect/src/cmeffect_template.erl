-module(cmeffect_template).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> template.

effect_apply(#{ template := Template, data := Data}, Id) ->
    Res = case cmtemplate:render(Template, Data) of 
              {ok, Data2} ->
                  #{ template => Template,
                     status => ok,
                     data => Data2 };
              {error, Error} -> 
                  #{ template => Template,
                     status => error,
                     reason => Error
                   }
          end,
    cmcore:update(Id, Res).
