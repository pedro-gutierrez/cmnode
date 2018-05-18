-module(cmdecode).
-export([decode/2, decode/3]).

decode(Spec, In) ->
    decode(Spec, In, #{}).

decode(#{ type := data }, Data, _) when is_binary(Data) ->
    {ok, Data};

decode(#{ type := object, spec := Spec }, Data, Config) ->
    decode_object(Spec, Data, Config, #{});

decode(#{ type := object }, Data, _) when is_map(Data) -> 
    {ok, Data};

decode(Spec, Data, Config) -> 
    decode_term(Spec, Data, Config).

decode_list(Spec, Data, Config) -> 
    decode_list(Spec, Data, Config, []).

decode_list(_, [], _, Out) -> {ok, lists:reverse(Out)};
decode_list(Spec, [Item|Rem], Config, Out) ->
    
    case decode(Spec, Item, Config) of 
        {ok, Decoded} ->
            decode_list(Spec, Rem, Config, [Decoded|Out]);
        Other -> 
            Other
    end.

decode_object(Spec, Data, Config, Out) ->
    decode_object(maps:keys(Spec), Spec, Data, Config, Out).

decode_object([], _, _, _, Out) -> {ok, Out};
decode_object([Key|Rem], Spec, Data, Config, Out) ->
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
            case decode_term(DecodeSpec, Value, Config ) of 
                {ok, Decoded} ->
                    decode_object(Rem, Spec, Data, Config, Out#{ Key => Decoded});
                no_match -> no_match
            end

    end.


decode_term(#{ type := keyword, value := Data}, Data, _) when is_atom(Data) -> {ok, Data};
decode_term(#{ type := keyword, spec := Spec}, Data, Config) when is_atom(Data) ->
    decode_term(Spec, Data, Config);

decode_term(#{ type := keyword, value := _}, _, _) -> no_match;
decode_term(#{ type := keyword }, Data, _) when is_atom(Data) -> {ok, Data}; 
decode_term(#{ type := text, value := Text}, Text, _) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text, value := _}, Text, _) when is_binary(Text) -> no_match;
decode_term(#{ type := text}, Text, _) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text}, Text, _) when is_list(Text) -> {ok, cmkit:to_bin(Text)};
decode_term(#{ type := number}, Num, _) when is_number(Num) -> {ok, Num};

decode_term(#{ type := list, spec := Spec }, Data, Config) when is_list(Data) -> 
    decode_list(Spec, Data, Config);


decode_term(#{ type := list, with := Spec}, Data, Config) when is_list(Data) and is_map(Spec) ->
    case cmencode:encode(Spec, Data, Config) of 
        {ok, Member} ->
            case lists:member(Member, Data) of
                true -> {ok, Data};
                false -> no_match
            end;
        Other -> Other
    end;


decode_term(#{ type := list, with := Member}, Data, _) when is_list(Data) ->
    case lists:member(Member, Data) of
        true -> {ok, Data};
        false -> no_match
    end;

decode_term(#{ type := list, without := Spec }, Data, Config) when is_list(Data) and is_map(Spec) ->
    case cmencode:encode(Spec, Data, Config) of 
        {ok, Member} ->
            case lists:member(Member, Data) of
                false -> {ok, Data};
                true -> no_match
            end;
        Other -> Other
    end;

decode_term(#{ type := list, without := Member}, Data, _) when is_list(Data) ->
    case lists:member(Member, Data) of
        false -> {ok, Data};
        true -> no_match
    end;

decode_term(#{ type := first, spec := Spec }, Data, Config) when is_list(Data) ->
    decode_first_item(Spec, Data, Config);

decode_term(#{ type := list}, List, _) when is_list(List) -> {ok, List};
decode_term(#{ type := email}, Email, _) ->
    case cmkit:is_email(Email) of 
        true -> {ok, Email};
        false -> no_match
    end;

decode_term(#{ type := object, spec := Spec}, In, Config) when is_map(In) ->
    decode_object(Spec, In, Config, #{});


decode_term(#{ type := object }, In, _) when is_map(In) ->
    {ok, In};

decode_term(#{ type := config, spec := Key}, _, Config)  ->
    case maps:get(Key, Config, undef) of 
        undef -> 
            cmkit:log({cmdecode, missing_config, Key, Config}),
            no_match;
        V -> {ok, V}
    end;

decode_term(#{ one_of := Specs }, In, Config) when is_list(Specs) ->
    decode_first_spec(Specs, In, Config);

decode_term(Spec, Data, _) -> 
    cmkit:log({cmdecode, not_implemented, Spec, Data}),
    no_match.

decode_first_spec([], _, _) -> no_match;
decode_first_spec([Spec|Rem], In, Config) ->
    case decode_term(Spec, In, Config) of 
        {ok, Decoded} -> {ok, Decoded};
        no_match ->
            decode_first_spec(Rem, In, Config)
    end.

decode_first_item(_, [], _) -> no_match;
decode_first_item(Spec, [Item|Rem], Config) ->
    case decode_term(Spec, Item, Config) of 
        {ok, Decoded} -> {ok, Decoded};
        no_match ->
            decode_first_item(Spec, Rem, Config)
    end.
