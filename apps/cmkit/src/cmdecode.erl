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
decode_object([Key|Rem], Spec, Data, Context, Out) when is_map(Data) ->
    KeySpec = maps:get(Key, Spec),
    Value = cmkit:value_at(Key, Data),
    case Value of 
        undef ->
            case decode_default(KeySpec, Data, Context) of 
                no_match ->
                    no_match;
                {ok, Decoded} ->
                    decode_object(Rem, Spec, Data, Context, Out#{ Key => Decoded})
            end;
        _ -> 
            case decode_term(KeySpec, Value, Context) of 
                {ok, Decoded} ->
                    decode_object(Rem, Spec, Data, Context, Out#{ Key => Decoded});
                no_match -> 
                    no_match
            end
    end.

decode_default(#{ default := Spec }, Data, Config) ->
    cmencode:encode(Spec, Data, Config);

decode_default(_, _, _) -> no_match.


decode_object_without_keys([], _, ok, In) -> {ok, In};
decode_object_without_keys([], _, false, _) -> no_match;

decode_object_without_keys([K|Rem], all, Result, In) ->
    case cmkit:value_at(K, In) of 
        undef -> decode_object_without_keys(Rem, all, Result, In);
        _ -> no_match
    end;

decode_object_without_keys([K|Rem], any, Result, In) ->
    case cmkit:value_at(K, In) of 
        undef -> 
            decode_object_without_keys(Rem, any, ok, In);
        _ ->
            decode_object_without_keys(Rem, any, Result, In)
    end.

decode_object_with_keys([], In) -> {ok, In};

decode_object_with_keys([K|Rem], In) ->
    case cmkit:value_at(K, In) of 
        undef -> 
            no_match;
        _ ->
            decode_object_with_keys(Rem, In)
    end.

decode_term(In, In, _) ->
    {ok, In};


decode_term(#{ type := boolean }, true, _) -> 
    {ok, true};

decode_term(#{ type := boolean }, false, _) -> 
    {ok, false};

decode_term(#{ type := without_keys, 
               match := Match,
               spec := KeysSpec}, In, Context) when is_map(In) -> 
    case cmencode:encode(KeysSpec, Context) of 
        {ok, Keys} ->
            InitialResult = case Match of 
                                any -> false;
                                all -> ok
                            end,

            decode_object_without_keys(Keys, Match, InitialResult, In);
        Other -> 
            Other
    end;

decode_term(#{ type := without_keys, spec := _}, _, _) ->  no_match;

decode_term(#{ type := with_keys, 
               spec := KeysSpec}, In, Context) when is_map(In) -> 

    case cmencode:encode(KeysSpec, Context) of 
        {ok, Keys} ->
            decode_object_with_keys(Keys, In);
        Other -> 
            Other
    end;

decode_term(#{ type := with_keys }, _, _) ->  no_match;

decode_term(#{ type := data }, Data, _) when is_binary(Data) -> {ok, Data};
decode_term(#{ type := data }, _, _) -> no_match;
decode_term(#{ type := keyword, value := Data}, Data, _) when is_atom(Data) -> {ok, Data};
decode_term(#{ type := keyword, spec := Spec}, Data, Config) when is_atom(Data) ->
    case cmencode:encode(Spec, Config) of 
        {ok, Data} ->
            {ok, Data};
        {ok, Encoded} when is_binary(Encoded) ->
            case cmkit:to_atom(Encoded) of 
                Data ->
                    {ok, Data};
                _ ->
                    no_match
            end;
        _ ->
            no_match
    end;

decode_term(#{ type := keyword, value := _}, _, _) -> no_match;
decode_term(#{ type := keyword }, Data, _) when is_atom(Data) -> {ok, Data}; 
decode_term(#{ type := keyword }, _, _) -> no_match; 
decode_term(#{ type := text, value := ValueSpec }, Text, Config) ->
    decode_term(#{ type => text,
                   spec => ValueSpec }, Text, Config);
decode_term(#{ type := text, spec := ValueSpec }, Text, Config) ->
    case decode(ValueSpec, Text, Config) of 
        {ok, Data} when is_binary(Data) ->
            {ok, Data};
        Other ->
            Other
    end;

decode_term(#{ type := text}, Text, _) when is_binary(Text) -> {ok, Text};
decode_term(#{ type := text}, Other, _) when is_list(Other) -> 
    case cmkit:is_string(Other) of 
        true ->
            {ok, cmkit:to_bin(Other)};
        false ->
            no_match
    end;

decode_term(#{ type := text}, _, _) -> no_match;

decode_term(#{ type := other_than, spec := #{ type := regexp, value := _ } = Spec}, Data, Config) -> 
    case decode_term(Spec, Data, Config) of 
        {ok, Data} -> 
            no_match;
        no_match -> 
            {ok, Data}
    end;

decode_term(#{ type := other_than, spec := Spec }, Data, Context) -> 
    case cmencode:encode(Spec, Context) of 
        {ok, Value} ->
            case decode_term(Value, Data, Context) of 
                no_match -> 
                    {ok, Data};
                {ok, _} ->
                    no_match
            end;
        Other ->
            cmkit:danger({cmdecode, other_than, Spec, Other}),
            no_match
    end;


decode_term(#{ type := in, 
               spec := Spec}, Data, Context) ->
    case cmencode:encode(Spec, Context) of 
        {ok, Map} when is_map(Map) ->
            case cmkit:value_at(Data, Map) of 
                undef ->
                    no_match;
                Value ->
                    {ok, Value}
            end;
        {ok, Other} ->
            cmkit:warning({cmdecode, in, Spec, Other}),
            no_match;
        _ ->
            no_match
    end;


decode_term(#{ type := lower_than, spec := Spec}, Data, Context) ->
    case cmencode:encode(Spec, Context) of
        {ok, Value} ->
            case Data < Value of 
                true ->
                    {ok, Data};
                false ->
                    no_match
            end;
        Other ->
            cmkit:danger({cmdecode, lower_than, Spec, Other}),
            no_match
    end;

decode_term(#{ type := greater_than, spec := Spec}, Data, Context) ->
    case cmencode:encode(Spec, Context) of
        {ok, Value} ->
            case Data > Value of 
                true ->
                    {ok, Data};
                false ->
                    no_match
            end;
        Other ->
            cmkit:danger({cmdecode, lower_than, Spec, Other}),
            no_match
    end;


decode_term(#{ type := regexp, value := Spec}=Spec0, Data, Config) -> 
    case cmencode:encode(Spec, Data, Config) of 
        {ok, EncodedRegex} ->
            case re:run(Data, EncodedRegex, [{capture, all_but_first, list}]) of 
                {match, Groups} ->
                    case maps:get(labels, Spec0, undef) of 
                        undef ->
                            {ok, Data};
                        LabelsSpec ->
                            case cmencode:encode(LabelsSpec, Config) of 
                                {ok, Labels} when is_list(Labels) andalso length(Labels) =:= length(Groups) ->
                                    cmkit:map_from(Labels, Groups,
                                                   maps:get(as, Spec0, binary));
                                Other ->
                                    cmkit:danger({cmdecode, regexp, incompatible_labels, Groups, Other}),
                                    no_match
                            end
                    end;
                nomatch -> 
                    no_match
            end;
        Other -> 
            cmkit:danger({cmdecode, regexp, Spec, Other}),
            no_match
    end;


decode_term(#{ type := number, value := Num}, Num, _) when is_number(Num) -> {ok, Num};
decode_term(#{ type := number, value := Num}, _, _) when is_number(Num) -> no_match;
decode_term(#{ type := number, value := Num}, Bin, _) when is_binary(Bin) ->
    case cmkit:to_number(Bin, none) of 
        Num -> {ok, Num};
        _ -> 
            no_match
    end;

decode_term(#{ type := number, spec := Spec}, Num, Config) ->
    decode_term(Spec, Num, Config);

decode_term(#{ type := number}, Num, _) when is_number(Num) -> {ok, Num};

decode_term(#{ type := number}, Bin, _) when is_binary(Bin) ->
    case cmkit:to_number(Bin, none) of 
        Num when is_number(Num) -> {ok, Num};
        _ -> 
            no_match
    end;

decode_term(#{ type := number}, _, _) -> no_match;


decode_term(#{ type := date, 
               format := iso8601 }, In, _) when is_binary(In) orelse is_list(In) ->

    case cmkit:parse_date(In) of 
        invalid -> no_match;
        Date -> {ok, Date}
    end;

decode_term(#{ type := date, 
               format := iso8601 }, _, _)  -> no_match;

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

decode_term(#{ type := empty }, <<>>, _) -> {ok, <<>>};
decode_term(#{ type := empty }, [], _) -> {ok, []};
decode_term(#{ type := empty }, Map, _) when is_map(Map) and map_size(Map) =:= 0 -> {ok, Map};
decode_term(#{ type := empty}, empty, _) -> {ok, empty};
decode_term(#{ type := empty}, _, _) -> no_match;

decode_term(#{ type := non_empty, spec := Spec }, Data, Config) -> 
    case decode_term(Spec, Data, Config) of 
        {ok, Decoded} when is_binary(Decoded) ->
            case size(Decoded) of 
                0 ->
                    no_match;
                _ -> 
                    {ok, Data}
            end;
        {ok, Other} ->
            cmkit:warning({decode, non_empty, not_implemented, Other}),
            no_match;
        _ ->
            no_match
    end;

decode_term(#{ type := list, spec := #{ size := Size }}, Data, _) when is_number(Size) and is_list(Data) -> 
    case length(Data) of 
        Size -> {ok, Data};
        _ -> no_match
    end;


decode_term(#{ type := list, 
               size := SizeSpec, 
               spec := Spec }, Data, Config) when is_list(Data) -> 
    case cmencode:encode(SizeSpec, Config) of 
        {ok, Size} ->
            case length(Data) of 
                Size ->
                    decode_list(Spec, Data, Config);
                _ -> 
                    no_match
            end;
        Other ->
            cmkit:danger({cmdecode, spec_encoding, SizeSpec, Other}),
            no_match
    end;

decode_term(#{ type := list, 
               size := SizeSpec }, Data, Config) when is_list(Data) -> 

    case cmencode:encode(SizeSpec, Config) of 
        {ok, Size} ->
            case length(Data) of 
                Size -> {ok, Data};
                _ -> no_match
            end;
        Other ->
            cmkit:danger({cmdecode, spec_encoding, SizeSpec, Other}),
            no_match
    end;

decode_term(#{ type := list, spec := _ }, [], _)  ->
    no_match;

decode_term(#{ type := list, spec := Spec }, Data, Config) when is_list(Data) ->
    decode_list(Spec, Data, Config);


decode_term(#{ type := list, with := Spec}, Data, Config) when is_list(Data)  ->
    case cmencode:encode(Spec, Config) of 
        {ok, Member} when is_map(Member) ->
            case decode_first_item(#{ type => object,
                                      spec => Member }, Data, Config) of 
                {ok, _} ->
                    {ok, Data};
                no_match ->
                    no_match
            end;
        {ok, Member} when is_list(Member) ->
            case decode_first_item(#{ type => list,
                                      value => Member }, Data, Config) of 
                {ok, _} ->
                    {ok, Data};
                no_match ->
                    no_match
            end;
        {ok, Member} ->
            case lists:member(Member, Data) of
                true -> {ok, Data};
                false -> no_match
            end;
        Other -> 
            cmkit:danger({cmdecode, spec_encoding, Spec, Other}),
            no_match
    end;

decode_term(#{ type := list, without := Spec}, Data, Config) when is_list(Data) ->
    case decode_term(Spec#{ type => list,
                            with => Spec }, Data, Config) of 
        no_match ->
            {ok, Data};
        {ok, _} ->
            no_match
    end;

decode_term(#{ type := first, spec := Spec }, Data, Config) when is_list(Data) ->
    decode_first_item(Spec, Data, Config);

decode_term(#{ type := first, spec := _}, _, _) ->
    no_match;

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

decode_term(#{ type := object, size := Size }, In, _) when (is_map(In) and map_size(In) =:= Size) ->
    {ok, In};

decode_term(#{ type := object, size := Size}, In, _) when (is_map(In) and map_size(In) =/= Size) ->
    no_match;

decode_term(#{ type := object }, In, _) when is_map(In) ->
    {ok, In};

decode_term(#{ type := object }, _, _) -> no_match;

decode_term(#{ type := config, spec := Spec}, Data, Config)  ->
    case cmencode:encode(Spec, Config) of 
        {ok, Expected} -> 
            decode_term(Expected, Data, Config); 
        Other -> 
            cmkit:danger({cmdecode, spec_encoding, Spec, Other}),
            no_match
    end;

decode_term(#{ one_of := Specs }, In, Config) when is_list(Specs) ->
    decode_first_spec(Specs, In, Config);

decode_term(#{ all := Specs }, In, Config) when is_list(Specs) ->
    decode_all_specs(Specs, In, Config);

decode_term(#{ type := member,
               spec := Spec }, In, Context) ->
    case cmencode:encode(Spec, Context) of
        {ok, Members} when is_list(Members) ->
            case lists:member(In, Members) of 
                true ->
                    {ok, In};
                false ->
                    no_match
            end;
        {ok, Members} when is_map(Members) ->
            case cmkit:value_at(In, Members) of 
                undef ->
                    no_match;
                Value ->
                    {ok, Value}
            end;
        {ok, Other} ->
            cmkit:warning({cmdecode, member, Spec, not_a_list, Other}),
            no_match;
        Other ->
            cmkit:danger({cmdecode, member, Spec, Other}),
            no_match
    end;

decode_term(#{ type := 'not', spec := Spec }, In, Config) ->
    case decode(Spec, In, Config) of 
        {ok, _} -> 
            no_match;
        no_match -> 
            {ok, In}
    end;

decode_term(Spec, Data, Context) when is_map(Spec) -> 
    case cmencode:encode(Spec, Context) of 
        {ok, Expected} when is_map(Expected) -> 
            decode_term(#{ type => object, spec => Expected }, Data, Context); 
        {ok, Expected} ->
            decode_term(Expected, Data, Context); 
        Other -> 
            cmkit:danger({cmencode, Spec, Other}),
            no_match
    end;

decode_term(V, V, _) -> {ok, V};

decode_term(B, Str, _) when is_list(Str) and is_binary(B) ->
    case cmkit:is_string(Str) of 
        true -> 
            case cmkit:to_bin(Str) of 
                B ->
                    {ok, B};
                _ ->
                    no_match
            end;
        false ->
            no_match
    end;

decode_term(_, _, _) ->
    no_match.


decode_first_spec([], _, _) -> no_match;
decode_first_spec([Spec|Rem], In, Config) ->
    case decode_term(Spec, In, Config) of 
        {ok, Decoded} -> {ok, Decoded};
        no_match ->
            decode_first_spec(Rem, In, Config)
    end.

decode_all_specs([], In, _) -> {ok, In};
decode_all_specs([Spec|Rem], In, Config) ->
    case decode_term(Spec, In, Config) of 
        {ok, _} -> 
            decode_all_specs(Rem, In, Config);
        no_match ->
            no_match
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
