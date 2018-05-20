-module(cmcore_util).
-export([
         init/2,
         cmds/4,
         context/1,
         update/5,
         update_spec/5,
         decode/3
        ]).

context(SessionId) ->
    case cmsession:retrieve(SessionId, context) of 
        {ok, [Pid]} -> {ok, Pid};
        Other -> Other
    end.

init(#{ init := Init }=App, Config) -> update(App, Init, Config);
init(_, _) -> {ok, #{}, []}.

update_spec(#{ update := Updates, encoders := Encoders }, Msg, Data, Model, Config) ->
    case maps:get(Msg, Updates, undef) of
        undef -> 
            {error, #{  update => not_implemented, msg => Msg }};
        Clauses ->
            In = #{ model => Model, data => Data },
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
    end.

decode(#{ decoders := Decoders }, Data, Config) ->
    decode(Decoders, Data, Config);

decode([], _, _) -> {error, no_match};

decode([#{ msg := Msg, spec := Spec}|Rem], Data, Config) ->
    case cmdecode:decode(Spec, Data, Config) of
        no_match -> 
            decode(Rem, Data, Config);
        {ok, Decoded} ->
            {ok, Msg, Decoded}
    end;

decode(_, _, _) -> {error, no_match}.

update(App, Spec, Config) -> update(App, Spec, Config, #{}).

update(App, Spec, Config, In) -> update(App, Spec, Config, In, {#{}, []}).

update(App, #{ model := M, cmds := C }, Config, In, {Model, Cmds}) ->
    case update_model(M, In, Config, Model) of 
        {ok, M2} -> 
            case resolve_cmds(App, C) of 
                {ok, C2} ->
                    {ok, M2, Cmds ++ C2};
                {error, E} -> 
                    {error, E}
            end;
        {error, E} -> {error, E}
    end;

update(_, Spec, _, _, _) -> {error, {invalid_update, Spec}}.

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


update_model(Spec, In, Config, Out) ->
    cmencode:encode(Spec, In, Config, Out).

cmds([], _, _, _) -> ok;
cmds([#{ effect := Effect, 
         encoder := Spec }|Rem], Model, Config, Session) ->
    case cmencode:encode(Spec, Model, Config) of 
        {error, Error} ->
            cmkit:log({cmcore, Effect, Spec, Error});
        {ok, Data} ->
            apply_effect(Effect, Data, Session)
    end,
    cmds(Rem, Model, Config, Session);

cmds([#{ effect := Effect}|Rem], Model, Config, Session) ->
    apply_effect(Effect, nothing, Session), 
    cmds(Rem, Model, Config, Session).

apply_effect(Effect, Data, #{ id := Id }=Session) ->
    case cmeffect:apply(Effect, Data, Session) of 
        ok -> ok;
        not_found ->
            cmcore:update(Id, #{ error => no_such_effect,
                                 effect => Effect,
                                 data => Data }),
            ok
    end.
