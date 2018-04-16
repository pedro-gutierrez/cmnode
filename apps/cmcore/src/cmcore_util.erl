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

init(Modules) ->
    init(Modules, { #{}, []}).

init([], {Model, Cmds}) -> {ok, Model, Cmds};
init([#{ init := Spec }|Rem]=Modules, Init) ->
    case update(Modules, Spec, #{}, Init) of 
        {ok, Model, Cmds} ->
            init(Rem, {Model, Cmds});
        Other -> Other
    end;

init([_|Rem], Init) ->
    init(Rem, Init).

update_spec([], _, Msg) -> 
    Error = #{  update => not_implemented,
                msg => Msg },
    {error, Error};

update_spec([Mod|Rem], #{ update := Update}, Msg) ->
    case maps:get(Msg, Update, undefined) of
        undefined ->
            update_spec(Rem, Mod, Msg);    
        Spec -> {ok, Spec}
    end;

update_spec([Mod|Rem], _, Msg) ->
    update_spec(Rem, Mod, Msg).

decode([], Data) -> 
    Error = #{ status => no_match,
               data => Data },
    {error, Error};

decode([#{ decoders := Decoders }=Mod|Rem], Data) ->
    case match(Decoders, Data) of 
        {error, no_match} -> 
            decode(Rem, Data);
        {ok, Msg, Decoded} -> 
            {ok, Mod, Msg, Decoded}
    end;

decode([_|Rem], Data) -> decode(Rem, Data).

match([], _) -> {error, no_match};
match([#{ msg := Msg, spec := Spec}|Rem], Data) ->
    case cmdecode:decode(Spec, Data) of
        no_match -> 
            match(Rem, Data);
        {ok, Decoded} ->
            {ok, Msg, Decoded}
    end;

match([_|Rem], Data) -> match(Rem, Data).

update(Modules, #{ model := M, cmds := C }, In, {Model, Cmds}) ->
    case update_model(M, In, Model) of 
        {ok, M2} -> 
            case resolve_cmds(Modules, C) of 
                {ok, C2} ->
                    {ok, M2, Cmds ++ C2};
                {error, E} -> 
                    {error, E}
            end;
        {error, E} -> {error, E}
    end;
        
update(_ ,_, _, _) -> {error, unknown_update_spec}.

resolve_cmds(Modules, Cmds) ->
    resolve_cmds(Modules, Cmds, []).

resolve_cmds(_, [], Out) -> {ok, lists:reverse(Out)};
resolve_cmds(Modules, [Cmd|Rem], Out) ->
    case resolve_cmd(Modules, Cmd) of 
        {ok, Cmd2} ->
            resolve_cmds(Modules, Rem, [Cmd2|Out]);
        {error, E} -> {error, E}
    end.

resolve_cmd([], Cmd) -> 
    {error, #{ status => no_such_encoder,
               cmd => Cmd}
    };

resolve_cmd([#{ encoders := Encoders }|Rem], #{ encoder := Enc }=Cmd) ->
    case maps:get(Enc, Encoders, undef) of
        undef ->
            resolve_cmd(Rem, Cmd);
        Encoder ->
            {ok, Cmd#{ encoder => Encoder }}
    end;

resolve_cmd(_, #{ effect := _ }=Cmd) ->
    {ok, Cmd };

resolve_cmd([_|Rem], Cmd) -> resolve_cmd(Rem, Cmd).

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
