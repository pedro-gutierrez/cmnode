-module(cmeffect_shell).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> shell.
effect_apply(#{ context := C,
                cmd :=  Cmd,
                chwd := Chwd }, SessionId) ->

    Res = case cmsh:sh(Cmd, [{cd, Chwd}]) of
              {ok, []} -> 
                  #{ output => <<>> };
              {ok, Out} when is_binary(Out) ->
                  #{ output => Out };
              {ok, Out} when is_list(Out) ->
                  #{ output => cmkit:to_bin(Out) };
              {error, {_, E}} ->
                  #{ error => cmkit:to_bin(E) }
          end,

    cmcore:update(SessionId, Res#{ context => C }).
