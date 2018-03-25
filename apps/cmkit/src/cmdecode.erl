-module(cmdecode).
-export([decode/2]).

decode(#{ type := data }, Data) when is_binary(Data) ->
    {ok, Data};

decode(#{ type := object, spec := Spec }, Data) ->
    decode_object(Spec, Data, #{});

decode(_, _) ->  no_match.

decode_object(Spec, Data, Out) ->
    decode_object(maps:keys(Spec), Spec, Data, Out).

decode_object([], _, _, Out) -> {ok, Out};
decode_object([Key|Rem], Spec, Data, Out) ->
    KeySpec = maps:get(Key, Spec),
    Value = maps:get(Key, Data, undef),
    case decode_term(KeySpec, Value) of 
        {ok, Decoded} ->
            decode_object(Rem, Spec, Data, Out#{ Key => Decoded});
        no_match -> no_match
    end.

decode_term(#{ type := keyword, value := Exp}, Exp) when is_atom(Exp) -> {ok, Exp};
decode_term(#{ type := keyword, value := _}, _) -> no_match;
decode_term(#{ type := text}, Text) when is_binary(Text) -> {ok, Text};

decode_term(Spec, Data) -> 
    cmkit:log({cmdecode, not_implemented, Spec, Data}),
    no_match.
