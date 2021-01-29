-module(cmeffect_archive).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> archive.
effect_apply(#{ context := C,
                archive := Archive,
                dir := Dir }, SessionId) ->

    Res = case cmkit:tar(Archive, Dir) of
              ok -> 
                  #{ status => success };
              {error, E} ->
                  #{ status => error,
                     error => E }
          end,

    cmcore:update(SessionId, Res#{ context => C }).
