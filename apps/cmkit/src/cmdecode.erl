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

decode_object(Spec, Data, Config, Out) when is_map(Data) ->
    decode_object(maps:keys(Spec), Spec, Data, Config, Out);

decode_object(_, _, _, _)  ->
    no_match.

decode_object([], _, _, _, Out) -> {ok, Out};
decode_object([Key|Rem], Spec, Data, Config, Out) when is_map(Data) ->
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

decode_term(#{ type := data }, Data, _) when is_binary(Data) -> {ok, Data};
decode_term(#{ type := data }, _, _) -> no_match;
decode_term(#{ type := keyword, value := Data}, Data, _) when is_atom(Data) -> {ok, Data};
decode_term(#{ type := keyword, spec := Spec}, Data, Config) when is_atom(Data) ->
    decode_term(Spec, Data, Config);

decode_term(#{ type := keyword, value := _}, _, _) -> no_match;
decode_term(#{ type := keyword }, Data, _) when is_atom(Data) -> {ok, Data}; 
decode_term(#{ type := keyword }, _, _) -> no_match; 
decode_term(#{ type := text, value := Text}, Text, _) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text, value := _}, Text, _) when is_binary(Text) -> no_match;
decode_term(#{ type := text}, Text, _) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text}, Text, _) when is_list(Text) -> 
    case cmkit:is_string(Text) of 
        true -> 
            {ok, cmkit:to_bin(Text)};
        false -> 
            no_match
    end;

decode_term(#{ type := text}, _, _) -> no_match;
decode_term(#{ type := regexp, value := Spec}, Data, Config) -> 
    case cmencode:encode(Spec, Data, Config) of 
        {ok, EncodedRegex} ->
            case re:run(Data, EncodedRegex) of 
                {match, _} -> 
                    {ok, Data};
                nomatch -> 
                    nomatch
            end;
        Other -> 
            cmkit:danger({cmdecode, regexp, Spec, Other}),
            no_match
    end;


decode_term(#{ type := number, value := Num}, Num, _) when is_number(Num) -> {ok, Num};
decode_term(#{ type := number, value := Num}, _, _) when is_number(Num) -> no_match;
decode_term(#{ type := number, value := Num}, Bin, _) when is_binary(Bin) ->
    case cmkit:to_number(Bin, none) of 
        none -> no_match;
        Num -> {ok, Num}
    end;

decode_term(#{ type := number, spec := Spec}, Num, Config) ->
    decode_term(Spec, Num, Config);

decode_term(#{ type := number}, Num, _) when is_number(Num) -> {ok, Num};

decode_term(#{ type := number}, Bin, _) when is_binary(Bin) ->
    case cmkit:to_number(Bin, none) of 
        none -> no_match;
        Num -> {ok, Num}
    end;

decode_term(#{ type := number}, _, _) -> no_match;

decode_term(#{ type := greater_than, spec := Min }, Num, _) when is_number(Min) and is_number(Num) ->
    case Num >= Min of 
        true -> {ok, Num};
        false -> no_match
    end;

decode_term(#{ type := greater_than, spec := Spec }, Num, Config) when is_map(Spec) and is_number(Num) ->
    case cmencode:encode(Spec, Config) of 
        {ok, Min} when is_number(Min) ->
            case Num >= Min of 
                true -> {ok, Num};
                false -> no_match
            end;
        Other -> 
            cmkit:danger({cmdecode, greatear_than, Spec, not_a_number, Other}),
            no_match
    end;
    
decode_term(#{ type := greater_than }, _, _) -> no_match;

decode_term(#{ type := empty}, empty, _) -> {ok, empty};
decode_term(#{ type := empty}, _, _) -> no_match;


decode_term(#{ type := list, spec := #{ size := Size }}, Data, _) when is_number(Size) and is_list(Data) -> 
    case length(Data) of 
        Size -> {ok, Data};
        _ -> no_match
    end;

decode_term(#{ type := list, 
               size := Size, 
               spec := Spec }, Data, Config) when is_list(Data) -> 
    case length(Data) of 
        Size ->  decode_list(Spec, Data, Config);
        _ -> no_match
    end;

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

decode_term(#{ type := list, value := Specs }, Data, Config) ->
    decode_all_items(Specs, Data, Config, []);

decode_term(#{ type := list}, List, _) when is_list(List) -> {ok, List}; 

decode_term(#{ type := list}, _, _ ) -> no_match;


decode_term(#{ type := email}, Email, _) ->
    case cmkit:is_email(Email) of 
        true -> {ok, Email};
        false -> no_match
    end;

decode_term(#{ type := object, spec := Spec}, In, Config) when is_map(In) ->
    decode_object(Spec, In, Config, #{});


decode_term(#{ type := object }, In, _) when is_map(In) ->
    {ok, In};

decode_term(#{ type := object }, _, _) -> no_match;

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
    cmkit:danger({cmdecode, not_implemented, Spec, Data}),
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

decode_all_items([], _, _, Out) -> {ok, lists:reverse(Out)};
decode_all_items(Specs, [], _, _) when length(Specs) > 0 -> no_match;
decode_all_items([Spec|Rem], Data, Config, Out) ->
    case decode_term(#{ type => first, spec => Spec }, Data, Config) of 
        no_match -> no_match;
        {ok, Decoded} ->
            decode_all_items(Rem, Data, Config, [Decoded|Out])
    end.
