-module(cmcore).
-export([init/4, update/2, update/6, notify/2, terminate/2]).


init(Pid, #{ name := App, 
             spec := Spec,
             config := Config0 }, Log, Effects) ->
    Config = case maps:get(encoders, Spec, undef) of
                 undef -> Config0;
                 Encs -> Config0#{ encoders => Encs }
             end,

    case init(Spec, Config) of
        {ok, Model, []} ->
            {ok, Model, Config};
        {ok, Model, Cmds} ->
            cmds(Cmds, Model, Config, Pid, Log, Effects),
            {ok, Model, Config};
        {error, E} ->
            err(App, Pid, init, E)
    end.

init(#{ init := Init }=App, Config) -> update(App, Init, Config);
init(_, _) -> {ok, #{}, []}.

update(App, Spec, Config) -> update(App, Spec, Config, #{}).

update(App, Spec, Config, In) -> update(App, Spec, Config, In, {#{}, []}).

update(App, #{ model := M, cmds := C } = Spec, Config, In, {Model, Cmds}) ->
    case data_with_where(Spec, In, Model, Config) of 
        {ok, In2} ->
            case cmencode:encode(M, In2, Config) of
                {ok, M2} ->
                    M3 = maps:merge(Model, M2),
                    case resolve_cmds(App, C) of
                        {ok, C2} ->
                            {ok, M3, Cmds ++ C2};
                        {error, E} ->
                            {error, E}
                    end;
                {error, E} -> 
                    {error, E}
            end;
        Other ->
            Other
    end;

update(_, Spec, _, _, _) -> {error, {invalid_update, Spec}}.

data_with_where(#{ where := WhereSpec }, Data, Model, Config) ->
    In = maps:merge(Model, Data),
    case cmencode:encode(WhereSpec, In, Config) of 
        {ok, Where} ->
            {ok, maps:merge(In, Where)};
        Other ->
            Other
    end;
        
data_with_where(_, Data, Model, _) -> {ok, maps:merge(Model, Data)}.

resolve_cmds(App, Cmds) ->
    resolve_cmds(App, Cmds, []).

resolve_cmds(_, [], Out) -> {ok, lists:reverse(Out)};
resolve_cmds(App, [Cmd|Rem], Out) ->
    case resolve_cmd(App, Cmd) of
        {ok, Cmd2} ->
            resolve_cmds(App, Rem, [Cmd2|Out]);
        {error, E} -> {error, E}
    end.

resolve_cmd(#{ encoders := Encoders }, #{ encoder := Enc }=Cmd) ->
    case maps:get(Enc, Encoders, undef) of
        undef ->
            {error, unknown_encoder(Enc) };
        Encoder ->
            {ok, Cmd#{ encoder => Encoder }}
    end;

resolve_cmd(_, #{ encoder := Enc }) ->
    {error, unknown_encoder(Enc)};

resolve_cmd(_, #{ effect := _ }=Cmd) ->
    {ok, Cmd };

resolve_cmd(_, Cmd) ->
    {error, {invalid_cmd, Cmd}}.

unknown_encoder(Enc) ->
    #{ encoder => Enc,
       status => undefined }.


cmds([], _, _, _, _, _) -> ok;
cmds([#{ effect := Effect,
         encoder := Spec }|Rem], Model, Config, Pid, Log, Effects) ->
    case cmencode:encode(Spec, Model, Config) of
        {error, Error} ->
            cmkit:danger({cmcore, Effect, Spec, Error});
        {ok, Data} ->
            apply_effect(Effect, Data, Pid, Log, Effects)
    end,
    cmds(Rem, Model, Config, Pid, Log, Effects);

cmds([#{ effect := Effect}|Rem], Model, Config, Pid, Log, Effects) ->
    apply_effect(Effect, #{}, Pid, Log, Effects),
    cmds(Rem, Model, Config, Pid, Log, Effects).

apply_effect(Effect, Data, Pid, _Log, Effects) ->
    case maps:get(Effect, Effects, undef) of
        undef ->
            cmkit:danger({cmcore, not_such_effect, Effect, Data});
        Mod ->
            spawn(fun() ->
                          Mod:effect_apply(Data, Pid)
                  end)
    end,
    ok.

update_spec(#{ update := Updates, encoders := Encoders }, Msg, Data, Model, Config) ->
    case maps:get(Msg, Updates, undef) of
        undef ->
            {error, #{  update => not_implemented, msg => Msg }};
        Clauses ->
            %In = #{ model => Model, data => Data },
            In = maps:merge(Model, Data),
            case first_clause(Clauses, Encoders, In, Config) of
                none ->
                    {error, #{  update => none_applies, msg => Msg }};
                Spec ->
                    {ok, Spec}
            end
    end;

update_spec(_, Msg, _, _, _) ->
    {error, #{ update => not_implemented, msg => Msg }}.

first_clause([], _, _, _) -> none;
first_clause([#{ condition := Cond}=Spec|Rem], Encoders, In, Config) ->
    case cmeval:eval(Cond, Encoders, In, Config) of
        true -> Spec;
        false -> first_clause(Rem, Encoders, In, Config)
    end;

first_clause([Spec|_], _, _, _) ->
    Spec.


decode(#{ decoders := Decoders }, #{ effect := default,
                                     data :=  Data }, Config) ->
    
    DefaultDecoders = maps:get(default, Decoders, []),
    decode_with(DefaultDecoders, Data, Config);
    
decode(#{ decoders := Decoders }, #{ effect := Effect,
                                     data :=  Data }, Config) ->
    case maps:get(Effect, Decoders, undef) of 
        undef ->
            DefaultDecoders = maps:get(default, Decoders, []),
            decode_with(DefaultDecoders, Data, Config);
        EffectDecoders ->
            case decode_with(EffectDecoders, Data, Config) of 
                {ok, Msg, Decoded} ->
                    {ok, Msg, Decoded};
                {error, no_match} ->
                    DefaultDecoders = maps:get(default, Decoders, []),
                    decode_with(DefaultDecoders, Data, Config)
            end
    end;

decode(#{ decoders := Decoders }, Data, Config) ->
    DefaultDecoders = maps:get(default, Decoders, []),
    decode_with(DefaultDecoders, Data, Config).

decode_with([], _, _) -> {error, no_match};

decode_with([#{ msg := Msg, spec := Spec}|Rem], Data, Config) ->
    case cmdecode:decode(Spec, Data, Config) of
        no_match ->
            decode_with(Rem, Data, Config);
        {ok, Decoded} ->
            {ok, Msg, Decoded}
    end;

decode_with(_, _, _) -> {error, no_match}.


update(Pid, #{ name := App,
               spec := Spec,
               config := Config } = AppSpec, Data, Model, Log, Effects) ->
    Log({App, Pid, in, Data}),
    case decode(Spec, Data, Config) of
        {ok, Msg, Decoded} ->
            Log({App, Pid, decoded, Msg}),
            update(Pid, AppSpec,  Msg, Decoded, Model, Log, Effects);
        {error, no_match} ->
            cmkit:warning({App, Pid, no_match, cmkit:printable(Data)}),
            update(Pid, AppSpec,  no_match, Data, Model, Log, Effects);
        {error, E} ->
            err(App, Pid, update, #{ data => Data, reason => E })
    end.

update(Pid, #{ name := App,
               spec := Spec,
               config := Config }, Msg, Data, Model, Log, Effects) ->
    
    case update_spec(Spec, Msg, Data, Model, Config) of
        {ok, UpdateSpec} ->
            case update(Spec, UpdateSpec, Config, Data, {Model, []}) of
                {ok, Model2, Cmds } ->
                    cmds(Cmds, Model2, Config, Pid, Log, Effects),
                    {ok, Model2};
                {error, E} ->
                    err(App, Pid, update, #{ data => Data, reason => E})
            end;
        {error, E} ->
            err(App, Pid, update, #{ data => Data, reason => E})
    end.

update(Pid, Data) when is_pid(Pid) ->
    Pid ! {update, Data}.

notify(Pid, Data) when is_pid(Pid) ->
    Pid ! Data.

terminate(Pid, Data) when is_pid(Pid) ->
    Pid ! {terminate, Data}.


err(App, Pid, Phase, Info) ->
    E = #{ status => error,
           app => App,
           pid => Pid,
           phase => Phase,
           info => Info },
    {error, E}.

