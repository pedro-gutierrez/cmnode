-module(cmeffect_s3).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> s3.
effect_apply(#{ context := C,
                access_key := _,
                secret_key := _,
                timeout := _,
                host := _,
                region := _,
                bucket := _,
                key := _,
                data := _ } = Spec, SessionId) ->

    Res = case cms3:put(Spec) of
            ok -> 
                  #{ status => success };
            {error, E} ->
                  #{ status => error,
                     error => E }
          end,

    cmcore:update(SessionId, Res#{ context => C });

effect_apply(#{ context := C,
                access_key := _,
                secret_key := _,
                timeout := _,
                host := _,
                region := _,
                bucket := _,
                key := _,
                save_as := ToFile} = Spec, SessionId) ->

    Res = case cms3:get(Spec) of
            {ok, Data } -> 
                  case file:write_file(cmkit:to_list(ToFile), Data) of 
                    ok ->
                        #{ status => success };
                    {error, E} ->
                        #{ status => error,
                           error => E }
                  end;
            {error, E} ->
                  #{ status => error,
                     error => E }
          end,

    cmcore:update(SessionId, Res#{ context => C });

effect_apply(#{ context := C,
                access_key := _,
                secret_key := _,
                timeout := _,
                host := _,
                region := _,
                bucket := _,
                key := _ } = Spec, SessionId) ->

    Res = case cms3:get(Spec) of
            {ok, Data } -> 
                  #{ status => success,
                     data => Data };
            {error, E} ->
                  #{ status => error,
                     error => E }
          end,

    cmcore:update(SessionId, Res#{ context => C }).
