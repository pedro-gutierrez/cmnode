-module(cmcore_util).
-export([
         init/1,
         cmds/3,
         context/1,
         update/4,
         update_spec/3,
         decode/2
        ]).

context(SessionId) ->
    case cmsession:retrieve(SessionId, context) of 
        {ok, [Pid]} -> {ok, Pid};
        Other -> Other
    end.

init(#{ init := Init }=App) -> update(App, Init);
init(_) -> {ok, #{}, []}.

update_spec(#{ update := Updates}, Msg, Model) ->
    case maps:get(Msg, Updates, undef) of
        undef -> 
            {error, #{  update => not_implemented, msg => Msg }};
        Specs ->
            case first_spec(Specs, Model) of 
                none -> 
                    {error, #{  update => none_applies, msg => Msg }};
                Spec ->
                    {ok, Spec}
            end
    end;

update_spec(_, Msg, _) ->
    {error, #{ update => not_implemented, msg => Msg }}.

first_spec([], _) -> none;
first_spec([#{ condition := Cond}=Spec|Rem], Model) ->
    case cmeval:eval(Cond, Model) of 
        true -> Spec;
        false -> first_spec(Rem, Model)
    end.

decode(#{ decoders := Decoders }, Data) ->
    decode(Decoders, Data);

decode([], _) -> {error, no_match};

decode([#{ msg := Msg, spec := Spec}|Rem], Data) ->
    case cmdecode:decode(Spec, Data) of
        no_match -> 
            decode(Rem, Data);
        {ok, Decoded} ->
            {ok, Msg, Decoded}
    end;

decode(_, _) -> {error, no_match}.

update(App, Spec) -> update(App, Spec, #{}).

update(App, Spec, In) -> update(App, Spec, In, {#{}, []}).

update(App, #{ model := M, cmds := C }, In, {Model, Cmds}) ->
    case update_model(M, In, Model) of 
        {ok, M2} -> 
            case resolve_cmds(App, C) of 
                {ok, C2} ->
                    {ok, M2, Cmds ++ C2};
                {error, E} -> 
                    {error, E}
            end;
        {error, E} -> {error, E}
    end;

update(_, Spec, _, _) -> {error, {invalid_update, Spec}}.



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
            {error, {no_such_encoder, Enc}};
        Encoder ->
            {ok, Cmd#{ encoder => Encoder }}
    end;

resolve_cmd(_, #{ encoder := Enc }) ->
    {error, {no_such_encoder, Enc}};

resolve_cmd(_, #{ effect := _ }=Cmd) ->
    {ok, Cmd };

resolve_cmd(_, Cmd) ->
    {error, {invalid_cmd, Cmd}}.





update_model(Spec, In, Out) ->
    resolve(Spec, In, Out).

resolve(#{ type := object, spec := Spec}, In, Out) ->
    resolve_object_values(maps:keys(Spec), Spec, In, Out); 

resolve(#{ type := data, from := Key}, In, _) when is_atom(Key) ->
    resolve_value_at(Key, In);

resolve(Enc, _, _) when is_atom(Enc) ->
    {error, #{ encoder => Enc,
               status => not_implemented }}.

resolve_object_values([], _, _, Out) -> {ok, Out};
resolve_object_values([K|Rem], Spec, In, Out) ->
    case resolve_value(maps:get(K, Spec), In) of 
        {ok, V} ->
            resolve_object_values(Rem, Spec, In, Out#{ K => V });
        Other -> Other
    end.

resolve_value_at(Key, In) ->
    case cmkit:value_at(Key, In) of
       undef ->
           {error, #{ status => missing_key,
                      key => Key,
                      data => In }};
       V ->
           {ok, V}
   end.

resolve_value(#{ from := Key, at := At }, In) when is_atom(Key) and is_atom(At) -> 
    case resolve_value_at(At, In) of 
        {ok, In2} ->
            resolve_value_at(Key, In2);
        Other -> 
            Other
    end;

resolve_value(#{ from := Key, at := At }, In) when is_atom(Key) and is_map(At) -> 
    case resolve_value(At, In) of 
        {ok, In2} ->
            resolve_value_at(Key, In2);
        Other -> 
            Other
    end;


resolve_value(#{ from := Key}, In) when is_atom(Key) -> 
    resolve_value_at(Key, In);


resolve_value(#{ type := text,
                 value := Value }, _) ->
    {ok, cmkit:to_bin(Value) };

resolve_value(#{ type := keyword,
                 value := Value }, _) when is_atom(Value) ->
    {ok, Value};

resolve_value(#{ type := object,
                 spec := Spec }, In) ->
    resolve_object_values(maps:keys(Spec), Spec, In, #{});

resolve_value(#{ type := list,
                 value := List }, In) when is_list(List)->
    {ok, lists:map(fun(V) ->
                           {ok, V2} = resolve_value(V, In),
                           V2 
                   end, List)};

resolve_value(#{ spec := Spec}, _) ->
    {ok, Spec}.

cmds([], _, _) -> ok;
cmds([#{ effect := Effect, 
         encoder := Spec }|Rem], Model, Session) ->
    case encode(Spec, Model) of 
        {error, Error} ->
            cmkit:log({cmcore, Effect, Spec, Error});
        {ok, Data} ->
            cmeffect:apply(Effect, Data, Session)
    end,
    cmds(Rem, Model, Session);

cmds([#{ effect := Effect}|Rem], Model, Session) ->
    cmeffect:apply(Effect, nothing, Session),
    cmds(Rem, Model, Session).

encode(Spec, Model) ->
    resolve(Spec, Model, #{}).
