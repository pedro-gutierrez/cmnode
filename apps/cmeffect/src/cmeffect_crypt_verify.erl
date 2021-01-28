-module(cmeffect_crypt_verify).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> crypt_verify.
effect_apply(#{ context := Context, 
                hash := Hash, 
                clear := Clear }, SessionId) ->

    Res = #{ context => Context,
             verified => Hash =:= Clear },
    cmcore:update(SessionId, Res).
