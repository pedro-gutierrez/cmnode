-module(cmcore).
-export([init/4, update/2, update/6, notify/2, terminate/2]).


init(Pid, #{ name := App, 
             spec := Spec,
             config := Config0 }, _Log, Effects) ->
    Config = case maps:get(encoders, Spec, undef) of
                 undef -> Config0;
                 Encs -> Config0#{ encoders => Encs }
             end,

    case init(Spec, Config) of
        {ok, Model, []} ->
            {ok, Model, Config};
        {ok, Model, Cmds} ->
            case cmds(Cmds, Model, Config, Pid, Effects) of 
                ok -> 
                    {ok, Model, Config};
                {error, E} ->
                    err(App, Pid, init, E)
            end;
        {error, E} ->
            err(App, Pid, init, E)
    end.

init(#{ init := Init }=App, Config) -> update(App, Init, Config);
init(_, _) -> {ok, #{}, []}.

update(App, Spec, Config) -> update(App, Spec, Config, #{}).

update(#{ encoders := Encs }, Spec, Config, In) -> 
    apply_update_spec(Encs, Spec, Config, In, {#{}, []}).

apply_update_spec(Encs, #{ model := M, cmds := C } = Spec, Config, In, {Model, Cmds}) ->
    case data_with_where(Spec, In, Model, Config) of 
        {ok, In2} ->
            case cmencode:encode(M, In2, Config) of
                {ok, M2} when is_map(M2) ->
                    M3 = maps:merge(Model, M2),
                    case resolve_cmds(Encs, C, M3) of
                        {ok, C2} ->
                            {ok, M3, Cmds ++ C2};
                        {error, E} ->
                            {error, E}
                    end;
                {ok, Other} ->
                    {error, #{ error => model_is_not_a_map,
                               spec => Spec,
                               data => Other }};
                {error, E} -> 
                    {error, E}
            end;
        Other ->
            Other
    end;

apply_update_spec( _, Spec, _, _, _) -> {error, {invalid_update, Spec}}.

data_with_where(#{ where := WhereSpec }, Data, Model, Config) ->
    In = maps:merge(Model, Data),
    case cmencode:encode(WhereSpec, In, Config) of 
        {ok, Where} ->
            {ok, maps:merge(In, Where)};
        Other ->
            Other
    end;

data_with_where(_, Data, Model, _) -> {ok, maps:merge(Model, Data)}.

resolve_cmds(Encoders, Cmds, Model) ->
    resolve_cmds(Encoders, Cmds, Model, []).

resolve_cmds(_, [], _, Out) -> {ok, lists:reverse(Out)};
resolve_cmds(Encoders, [Cmd|Rem], Model, Out) ->
    case resolve_cmd(Encoders, Cmd, Model) of
        {ok, Cmd2} ->
            resolve_cmds(Encoders, Rem, Model, [Cmd2|Out]);
        {error, E} -> {error, E}
    end;

resolve_cmds(_, Cmds, _, _) ->
    {error, #{ error => invalid_cmds,
               reason => not_a_list,
               cmds => Cmds }}.

resolve_cmd(Encoders, #{ encoder := Enc }=Cmd, _) when is_atom(Enc) orelse is_binary(Enc)->
    case encoder_named(Enc, Encoders) of
        {ok, Encoder} ->
            {ok, Cmd#{ encoder => Encoder }};
        Other ->
            Other
    end;

resolve_cmd(Encoders, #{ encoder := Expr }=Cmd, Model) when is_map(Expr) ->
    case cmencode:encode(Expr, Model) of 
        {ok, EncoderName} when is_binary(EncoderName) orelse is_atom(EncoderName) ->
            case encoder_named(EncoderName, Encoders) of 
                {ok, Encoder} ->
                    {ok, Cmd#{ encoder => Encoder }};
                Other ->
                    Other
            end;
        {ok, Other} ->
            {error, #{ error => invalid_encoder_name_expression,
                       encoder => Expr,
                       encoded_as => Other }};

        Error ->
            Error
    end;

resolve_cmd(_, #{ effect := _ }=Cmd, _) ->
    {ok, Cmd };

resolve_cmd(Encoders, CmdSpec, Model) ->
    case cmencode:encode(CmdSpec, Model) of 
        {ok, #{ encoder := _,
                effect:= _ } = Spec} ->
            resolve_cmd(Encoders, Spec, Model);

        {ok, #{ effect := _ } = Spec} ->
            resolve_cmd(Encoders, Spec, Model);
        {ok, Other} ->
            {error, #{ error => invalid_cmd,
                       spec => CmdSpec,
                       reason => Other }};
        Other ->
            {error, #{ error => invalid_cmd,
                       spec => CmdSpec,
                       reason => Other }}
    end.

encoder_named(Name, Encs) ->
    case cmkit:value_at(Name, Encs) of 
        undef -> 
            {error, unknown_encoder(Name, Encs)};
        Enc ->
            {ok, Enc}
    end.

unknown_encoder(Enc, Encoders) ->
    #{ encoder => Enc,
       error => no_such_encoder,
       encoders => maps:keys(Encoders) }.


resolve_effect(Expr, Effects, _) when is_binary(Expr) orelse is_atom(Expr) ->
    effect_named(Expr, Effects);

resolve_effect(Expr, Effects, Model) when is_map(Expr)->
    case cmencode:encode(Expr, Model) of 
        {ok, Name} when  is_binary(Name) orelse is_atom(Name) ->
            resolve_effect(Name, Effects, Model);
        {ok, Other} ->
            {error, #{ error => invalid_effect_name_expression,
                       effect => Expr,
                       encoded_as => Other }};
        Error ->
            Error
    end.


effect_named(Name, Effects) ->
    case cmkit:value_at(Name, Effects) of
        undef ->
            {error, unknown_effect(Name, Effects)};
        Mod ->
            {ok, Mod}
    end.

unknown_effect(Eff, Effects) ->
    #{ effect => Eff,
       error => no_such_effect,
       effects => maps:keys(Effects) }.



validated_cmds(Specs, Model, Config, Effects) ->
    validated_cmds(Specs, Model, Config, Effects, []).

validated_cmds([], _, _, _, Out) -> {ok, lists:reverse(Out)};

validated_cmds([#{ effect := Effect,
                   encoder := Spec }|Rem], Model, Config, Effects, Out) ->
    case resolve_effect(Effect, Effects, Model) of 
        {ok, Mod} ->
            case cmencode:encode(Spec, Model, Config) of 
                {ok, Params} ->
                    validated_cmds(Rem, Model, Config, Effects, [{Mod, Params}|Out]);
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

validated_cmds([#{ effect := Effect }|Rem], Model, Config, Effects, Out) ->
    case resolve_effect(Effect, Effects, Model) of 
        {ok, Mod} ->
            validated_cmds(Rem, Model, Config, Effects, [{Mod, #{}}|Out]);
        Other ->
            Other
    end.


cmds(Specs, Model, Config, Pid, Effects) ->
    case validated_cmds(Specs, Model, Config, Effects) of 
        {ok, Cmds} ->
            lists:foreach(fun({Mod, Params}) ->
                                  spawn(fun() ->
                                                Mod:effect_apply(Params, Pid)
                                        end)
                          end, Cmds),
            ok;
        Other ->
            Other
    end.

update_spec(#{ update := Updates }, Encoders, Msg, Data, Model, Config) ->
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

update_spec(_, _, Msg, _, _, _) ->
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
                                     data :=  Data }, Model, Config) ->

    DefaultDecoders = maps:get(default, Decoders, []),
    decode_with(DefaultDecoders, Data, Model, Config);

decode(#{ decoders := Decoders }, #{ effect := Effect,
                                     data :=  Data }, Model, Config) ->
    case maps:get(Effect, Decoders, undef) of 
        undef ->
            DefaultDecoders = maps:get(default, Decoders, []),
            decode_with(DefaultDecoders, Data, Model, Config);
        EffectDecoders ->
            case decode_with(EffectDecoders, Data, Model, Config) of 
                {ok, Msg, Decoded} ->
                    {ok, Msg, Decoded};
                {error, no_match} ->
                    DefaultDecoders = maps:get(default, Decoders, []),
                    decode_with(DefaultDecoders, Data, Model, Config)
            end
    end;

decode(#{ decoders := Decoders }, Data, Model, Config) ->
    DefaultDecoders = maps:get(default, Decoders, []),
    decode_with(DefaultDecoders, Data, Model, Config).

decode_with([], _, _, _) -> {error, no_match};

decode_with([#{ msg := Msg, spec := Spec}|Rem], Data, Model, Config) ->
    case cmdecode:decode(Spec, Data, maps:merge(Config, Model)) of
        no_match ->
            decode_with(Rem, Data, Model, Config);
        {ok, Decoded} ->
            {ok, Msg, Decoded}
    end;

decode_with(_, _, _, _) -> {error, no_match}.


with_encoders(Encs, #{ encoders := Encs2} = Spec) ->
    Spec#{ encoders => maps:merge(Encs, Encs2) }.

update(Pid, #{ name := App,
               filters := [Filter|RemFilters], 
               spec := #{ encoders := Encs },
               config := Config } = AppSpec, Data, Model, Log, Effects) ->
    Log({App, Pid, #{ in => Data, 
                      model => Model}}),
    case decode(Filter, Data, Model, Config) of
        {ok, Msg, Decoded} ->
            Log({App, Pid, decoded, Msg}),
            #{ encoders := Encs2 } = Filter2 = with_encoders(Encs, Filter),
            case update_spec(Filter2, Encs2, Msg, Decoded, Model, Config) of
                {ok, UpdateSpec} ->
                    case apply_update_spec(Encs2, UpdateSpec, Config, Decoded, {Model, []}) of
                        {ok, Model2, []} ->
                            Data0 = maps:get(data, AppSpec, Data),
                            AppSpec2 = AppSpec#{ data => Data0,
                                                 filters => RemFilters },
                            update(Pid, AppSpec2, Data0, Model2, Log, Effects);  
                        {ok, Model2, Cmds} ->
                            case cmds(Cmds, Model2, Config, Pid, Effects) of 
                                ok ->
                                    Data0 = maps:get(data, AppSpec, Data),
                                    AppSpec2 = AppSpec#{ data => Data0 },
                                    {ok, Model2, AppSpec2}; 
                                {error, E} ->
                                    err(App, Pid, cmds, #{ data => Data, reason => E})
                            end;
                        {error, E} ->
                            err(App, Pid, update, #{ data => Data, reason => E})
                    end;
                {error, E} ->
                    err(App, Pid, update, #{ data => Data, reason => E})
            end;
        {error, no_match} ->
            cmkit:warning({App, Pid, no_match, cmkit:printable(Data)}),
            err(App, Pid, filter, #{ data => Data, reason => no_match });
        {error, E} ->
            err(App, Pid, filter, #{ data => Data, reason => E })
    end;

update(_, #{ filters_only := true } = Spec, _, Model, _, _) ->
    {ok, Model, maps:without([data, filters, filters_only], Spec)};

update(Pid, #{ name := App,
               spec := Spec,
               config := Config } = AppSpec, Data0, Model, Log, Effects) ->
    Data = maps:get(data, AppSpec, Data0),
    AppSpec2 = maps:without([data], AppSpec),
    Log({App, Pid, #{ in => Data, model => Model}}),
    case decode(Spec, Data, Model, Config) of
        {ok, Msg, Decoded} ->
            Log({App, Pid, decoded, Msg}),
            update(Pid, AppSpec2,  Msg, Decoded, Model, Log, Effects);
        {error, no_match} ->
            cmkit:warning({App, Pid, no_match, cmkit:printable(Data)}),
            update(Pid, AppSpec2,  no_match, Data, Model, Log, Effects);
        {error, E} ->
            err(App, Pid, update, #{ data => Data, reason => E })
    end.

update(Pid, #{ name := App,
               spec := #{ encoders := Encs }=Spec,
               config := Config } = AppSpec, Msg, Data, Model, _Log, Effects) ->

    case update_spec(Spec, Encs, Msg, Data, Model, Config) of
        {ok, UpdateSpec} ->
            case apply_update_spec(Encs, UpdateSpec, Config, Data, {Model, []}) of
                {ok, Model2, Cmds } ->
                    case cmds(Cmds, Model2, Config, Pid, Effects) of 
                        ok ->
                            {ok, Model2, AppSpec};
                        {error, E} ->
                            err(App, Pid, cmds, #{ data => Data, reason => E})
                    end;
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

