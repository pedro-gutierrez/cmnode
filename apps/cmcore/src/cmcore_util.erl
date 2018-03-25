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

init([], Init) -> Init;
init([#{ init := Spec }|Rem]=Modules, Init) ->
    Init2 = update(Modules, Spec, #{}, Init),
    init(Rem, Init2).

update_spec([], _, Msg) -> 
    Error = #{ msg => Msg,
               status => undefined },
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
    { update_model(M, In, Model), Cmds ++ resolve_cmds(Modules, C) };
        
update(_, #{ model := M }, In, {Model, Cmds}) ->
    { update_model(M, In, Model), Cmds };    

update(Modules, #{ cmds := C }, _, {Model, Cmds}) ->
    { Model, Cmds ++ resolve_cmds(Modules, C) };

update(_ ,_, _, Out) -> Out.

resolve_cmds(Modules, Cmds) ->
    lists:map(fun(Cmd) ->
                      resolve_cmd(Modules, Cmd)
              end, Cmds).

resolve_cmd([], Cmd) -> Cmd;
resolve_cmd([#{ encoders := Encoders }|Rem], #{ encoder := Enc }=Cmd) ->
    case maps:get(Enc, Encoders, undef) of
        undef ->
            resolve_cmd(Rem, Cmd);
        Encoder ->
            Cmd#{ encoder => Encoder }
    end;

resolve_cmd([_|Rem], Cmd) -> resolve_cmd(Rem, Cmd).

update_model(Spec, In, Out) ->
    resolve(Spec, In, Out).

resolve(#{ type := object, spec := Spec}, In, Out) ->
    maps:fold( fun(K, V, Out2) ->
                       Out2#{ K => resolve_value(V, In) }
               end, Out, Spec);

resolve(#{ type := data, from := Key}, In, _) when is_atom(Key) ->
    maps:get(Key, In).

resolve_value(#{ from := Key}, In) when is_atom(Key) -> 
    maps:get(Key, In, undefined);

resolve_value(#{ type := text,
                 value := Value }, _) ->
    cmkit:to_bin(Value);

resolve_value(#{ type := keyword,
                 value := Value }, _) when is_atom(Value) ->
    Value;

resolve_value(#{ spec := Spec}, _) ->
    Spec.


cmds([], _, _) -> ok;
cmds([#{ effect := Effect, 
         encoder := Spec }|Rem], Model, Session) ->
    case encode(Spec, Model) of 
        {error, Error} ->
            cmkit:log({nkcore, Effect, Spec, Error});
        {ok, Data} ->
            cmeffect:apply(Effect, Data, Session)
    end,
    cmds(Rem, Model, Session).


encode(Spec, Model) ->
    {ok, resolve(Spec, Model, #{})}.





