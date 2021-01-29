-module(cmeffect_shell).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> shell.
effect_apply(#{ context := C,
                cmd :=  Cmd } = Spec, SessionId) ->

    Opts = case maps:get(chwd, Spec, undef) of 
               undef -> [];
               Dir ->
                   [{cd, Dir}]
           end,

    Res = case cmd(Cmd, Opts) of
              {ok, Out} -> 
                  #{ status => ok,
                     output => Out };
              {error, E} ->
                  #{ status => error,
                     error => E }
          end,

    cmcore:update(SessionId, Res#{ context => C });

effect_apply(#{ context := C,
                script:=  Script } = Spec, SessionId) ->

    Opts = case maps:get(chwd, Spec, undef) of 
               undef -> [];
               Dir ->
                   [{cd, Dir}]
           end,

    Script2 = case maps:get(params, Spec, undef) of 
                  undef -> 
                      Script;
                  Params when is_map(Params) ->
                      {ok, Script0} = cmkit:fmt_named(Script, Params),
                      Script0;
                  Params when is_list(Params) ->
                      cmkit:fmt(Script, Params)
              end,

    Res = case cmds(cmkit:bin_split(Script2, <<"\n">>), Opts, []) of 
              {ok, Out} -> 
                  #{ status => ok,
                     output => Out };
              {error, E} ->
                  #{ status => error,
                     error => E }
          end,

    cmcore:update(SessionId, Res#{ context => C }).



cmds([], _, Out) -> {ok, lists:reverse(Out)};
cmds([<<>>|Rest], Opts, Out) ->
    cmds(Rest, Opts, Out);
cmds([Cmd|Rest], Opts, Out) ->
    case cmd(Cmd, Opts) of 
        {ok, Out0} ->
            cmds(Rest, Opts, [Out0|Out]);
        Other ->
            Other
    end.

cmd(Cmd, Opts) ->
    cmkit:log({shell, Cmd}),

    case cmsh:sh(Cmd, Opts) of
        {ok, Out0} when is_list(Out0) ->
            {ok, cmkit:to_bin(Out0)};
        {ok, Out0} when is_binary(Out0) ->
            {ok, Out0};
        {error, {_, E}} ->
            {error, cmkit:to_bin(E)}
    end.
