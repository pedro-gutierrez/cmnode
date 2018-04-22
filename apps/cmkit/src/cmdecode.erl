-module(cmdecode).
-export([decode/2]).

decode(#{ type := data }, Data) when is_binary(Data) ->
    {ok, Data};

decode(#{ type := object, spec := Spec }, Data) ->
    decode_object(Spec, Data, #{});

decode(#{ type := object }, Data) when is_map(Data) -> 
    {ok, Data};

decode(#{ type := list } = Spec, Data) -> 
    decode_term(Spec, Data);

decode(_, _) ->  no_match.


decode_list(Spec, Data) -> 
    decode_list(Spec, Data, []).

decode_list(_, [], Out) -> {ok, lists:reverse(Out)};
decode_list(Spec, [Item|Rem], Out) ->
    case decode(Spec, Item) of 
        {ok, Decoded} ->
            decode_list(Spec, Rem, [Decoded|Out]);
        Other -> Other
    end.

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


decode_term(#{ type := keyword, value := Data}, Data) when is_atom(Data) -> {ok, Data};
decode_term(#{ type := keyword, spec := Spec}, Data) when is_atom(Data) ->
    decode_term(Spec, Data);

decode_term(#{ type := keyword, value := _}, _) -> no_match;
decode_term(#{ type := keyword }, Data) when is_atom(Data) -> {ok, Data}; 
decode_term(#{ type := text, value := Text, constraint := equal}, Text) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text, value := _, constraint := equal}, _) -> no_match;
decode_term(#{ type := text, value := Text}, Text) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text, value := _}, Text) when is_binary(Text) -> no_match;
decode_term(#{ type := text}, Text) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := list, spec := Spec }, Data) when is_list(Data) -> 
    decode_list(Spec, Data);

decode_term(#{ type := list, with := Member }, Data) when is_list(Data) ->
    cmkit:log({cmdecode, list, member, Member, Data}),
    case lists:member(Member, Data) of
        true -> {ok, Data};
        false -> no_match
    end;

decode_term(#{ type := list}, List) when is_list(List) -> {ok, List};
decode_term(#{ type := email}, Email) ->
    case cmkit:is_email(Email) of 
        true -> {ok, Email};
        false -> no_match
    end;

decode_term(#{ type := object, spec := Spec}, In) when is_map(In) ->
    decode_object(Spec, In, #{});

decode_term(#{ one_of := Specs }, In) when is_list(Specs) ->
    decode_first(Specs, In);

decode_term(Spec, Data) -> 
    cmkit:log({cmdecode, not_implemented, Spec, Data}),
    no_match.

decode_first([], _) -> no_match;
decode_first([Spec|Rem], In) ->
    case decode_term(Spec, In) of 
        {ok, Decoded} -> {ok, Decoded};
        no_match ->
            decode_first(Rem, In)
    end.

