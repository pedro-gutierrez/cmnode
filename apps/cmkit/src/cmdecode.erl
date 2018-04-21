-module(cmdecode).
-export([decode/2]).

decode(#{ type := data }, Data) when is_binary(Data) ->
    {ok, Data};

decode(#{ type := object, spec := Spec }, Data) ->
    decode_object(Spec, Data, #{});

decode(#{ type := object }, Data) when is_map(Data) -> 
    {ok, Data};

decode(_, _) ->  no_match.

decode_object(Spec, Data, Out) ->
    decode_object(maps:keys(Spec), Spec, Data, Out).

decode_object([], _, _, Out) -> {ok, Out};
decode_object([Key|Rem], Spec, Data, Out) ->
    KeySpec = maps:get(Key, Spec),
    Value = cmkit:value_at(Key, Data),
    case Value of 
        undef -> 
            no_match;
        _ -> 
            DecodeSpec = case maps:get(relates_to, KeySpec, undef) of 
                             undef -> 
                                 KeySpec;
                             OtherKey ->
                                 KeySpec#{ value => cmkit:value_at(OtherKey, Data) }
                         end,

            case decode_term(DecodeSpec, Value) of 
                {ok, Decoded} ->
                    decode_object(Rem, Spec, Data, Out#{ Key => Decoded});
                no_match -> no_match
            end

    end.


decode_term(#{ type := keyword, value := Exp}, Exp) when is_atom(Exp) -> {ok, Exp};
decode_term(#{ type := keyword, value := _}, _) -> no_match;
decode_term(#{ type := text, value := Text, constraint := equal}, Text) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text, value := _, constraint := equal}, _) -> no_match;
decode_term(#{ type := text, value := Text}, Text) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text, value := _}, Text) when is_binary(Text) -> no_match;
decode_term(#{ type := text}, Text) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := list}, List) when is_list(List) -> {ok, List};
decode_term(#{ type := email}, Email) ->
    case cmkit:is_email(Email) of 
        true -> {ok, Email};
        false -> no_match
    end;

decode_term(#{ type := object, spec := Spec}, In) when is_map(In) ->
    decode_object(Spec, In, #{});

decode_term(Spec, Data) -> 
    cmkit:log({cmdecode, not_implemented, Spec, Data}),
    no_match.
