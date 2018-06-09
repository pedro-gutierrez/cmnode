-module(cmencode).
-export([encode/1, 
         encode/2,
         encode/3]).

encode(Spec) -> encode(Spec, #{}).
encode(Spec, In) -> encode(Spec, In, #{}).


encode(#{ type := object, spec := Spec}, In, Config) ->
    encode_object(Spec, In, Config);
    
encode(#{ type := object }, _, _) ->
    {ok, #{}};

encode(#{ type := data, from := Key}, In, Config) when is_atom(Key) ->
    encode(Key, In, Config);

encode(Key, In, _) when is_atom(Key) or is_binary(Key) ->
    case cmkit:value_at(Key, In) of
       undef ->
            E = #{ status => missing_key,
                      key => Key,
                      data => In },
            cmkit:danger({cmencode, E}),
           
            {error, E};
       V ->
           {ok, V}
   end;


encode(#{ item := Num, in := At }, In, Config) when ( is_atom(At) or is_binary(At) or is_map(At)) ->
    case encode(At, In, Config) of 
        {ok, In2} ->
            case is_list(In2) of 
                false ->
                    {error, #{ status => not_a_list, 
                               at => At }};
                true ->
                    case Num =< length(In2) of 
                        false -> 
                            {error, #{ status => list_too_small,
                                       size => length(In2),
                                       looking_for => Num }};
                        true ->
                            {ok, lists:nth(Num, In2)}
                    end
            end;
        Other -> 
            Other
    end;

encode(#{ key := Key, in := At }, In, Config) when is_atom(Key) and ( is_atom(At) or is_binary(At))-> 
    case encode(At, In, Config) of 
        {ok, In2} ->
            encode(Key, In2, Config);
        Other -> 
            Other
    end;

encode(#{ key := Key, in := At }, In, Config) when is_atom(Key) and is_map(At) -> 
    case encode(At, In, Config) of 
        {ok, In2} ->
            encode(Key, In2, Config);
        Other -> 
            Other
    end;

encode(#{ key := Key}, In, Config) when is_atom(Key) -> 
    encode(Key, In, Config);

encode(#{ type := text, value := Value }, _, _) ->
    {ok, cmkit:to_bin(Value) };


encode(#{ type := number, value:= V }, _, _) when is_number(V) ->
    {ok, V};

encode(#{ type := member,
          spec := #{ value := ValueSpec,
                     in := CollectionSpec }}, In, Config) ->
    case encode(CollectionSpec, In, Config) of
        {ok, List} when is_list(List) ->
            case encode(ValueSpec, In, Config) of
                {ok, Member} ->
                    {ok, lists:member(Member, List)};
                Other ->
                    {error, #{ status => encode_error,
                               spec => ValueSpec,
                               data => In,
                               reason => Other
                             }
                    }
            end;
        Other ->
            {error, #{ status => encode_error,
                       spec => CollectionSpec,
                       data => In,
                       reason => Other
                     }
            }
    end;

encode(#{ type := text,
          spec := Spec }, In, Config) ->
    encode(Spec, In, Config);

encode(#{ type := keyword,
          value := Value }, _, _) when is_atom(Value) ->
    {ok, Value};

encode(#{ type := list,
          value := List }, In, Config) when is_list(List)->
    encode_list(List, In, Config);


encode(#{ type := config,
          spec := Key }, _, Config) ->
    {ok, maps:get(Key, Config)};


encode(#{ type := request,
         spec := Spec }, In, Config) ->
    encode(Spec, In, Config);

encode(#{ type := http,
          method := Method,
          body := Body,
          headers := Headers }, In, Config) ->
    case encode(Headers, In, Config) of 
        {ok, H} ->
            case encode(Body, In, Config) of 
                {ok, B} ->
                    {ok, #{ method => Method,
                            headers => H,
                            body => B }}; 
                Other -> Other
            end;
        Other -> Other
    end;

encode(#{ type := path,
          location := Path }, _, _) -> {ok, cmkit:to_list(Path)};

encode(#{ type := file,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of 
        {ok, Path} ->
            file:read_file(Path);
        Other -> 
            Other
    end;

encode(#{ type := base64,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Data} ->
            {ok, base64:encode(Data)};
        Other -> 
            Other
    end;

encode(#{ type := asset,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Name} ->
            file:read_file(filename:join(cmkit:assets(), Name));
        Other -> Other
    end;

encode(#{ type := greater_than,
          spec := [Spec1, Spec2] }, In, Config) ->
    case { encode(Spec1, In, Config), encode(Spec2, In, Config) } of 
        { {ok, V1}, {ok, V2} } when (is_number(V1) and is_number(V2)) ->
            {ok, V1 > V2};
        Other -> Other
    end;

encode(#{ type := sum,
          spec := Specs } = Spec, In, Config) ->
    Res = lists:foldl(fun({ok, V}, Total) when is_number(V) -> 
                        Total + V;
                   (Other, _) -> 
                        {error, #{ status => encode_error,
                                   spec => Spec,
                                   data => Other,
                                   reason => not_a_number }}
                end, 0, lists:map(fun(S) ->
                                       encode(S, In, Config)
                               end, Specs)),
    case Res of 
        N when is_number(N) -> {ok, N};
        Other -> Other
    end;

encode(#{ spec := Spec }, _, _) -> {ok, Spec}.

encode_object(Spec, In, Config) ->
    encode_object(maps:keys(Spec), Spec, In, Config, #{}).

encode_object([], _, _,_, Out) -> {ok, Out};
encode_object([K|Rem], Spec, In, Config, Out) ->
    case encode(maps:get(K, Spec), In, Config) of 
        {ok, V} ->
            encode_object(Rem, Spec, In, Config, Out#{ K => V });
        Other -> Other
    end.

encode_list(Specs, In, Config) ->
    encode_list(Specs, In, Config, []).

encode_list([], _, _, Out) -> {ok, lists:reverse(Out)};
encode_list([Spec|Rem], In, Config, Out) ->
    case encode(Spec, In, Config) of 
        {ok, Encoded} ->
            encode_list(Rem, In, Config, [Encoded|Out]);
        Other -> 
            fail_encoding(Spec, In, Other)
    end.

fail_encoding(Spec, In, Out) ->
    {error, #{ status => encode_error,
               spec => Spec,
               data => In,
               reason => Out 
             }
    }.

