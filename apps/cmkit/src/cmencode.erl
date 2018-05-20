-module(cmencode).
-export([encode/1, 
         encode/2,
         encode/3,
         encode/4]).

encode(Spec) -> encode(Spec, #{}).
encode(Spec, In) -> encode(Spec, In, #{}).
encode(Spec, In, Config) -> encode(Spec, In, Config, #{}).

encode(#{ type := object, spec := Spec}, In, Config, Out) ->
    encode(maps:keys(Spec), Spec, In, Config, Out);

encode(#{ type := data, from := Key}, In, Config, Out) when is_atom(Key) ->
    encode(Key, In, Config, Out);

encode(Key, In, _, _) when is_atom(Key) or is_binary(Key) ->
    case cmkit:value_at(Key, In) of
       undef ->
            E = #{ status => missing_key,
                      key => Key,
                      data => In },
            cmkit:danger(E),
           
            {error, E};
       V ->
           {ok, V}
   end;

encode(#{ key := Key, in := At }, In, Config, Out) when is_atom(Key) and ( is_atom(At) or is_binary(At))-> 
    case encode(At, In, Config, Out) of 
        {ok, In2} ->
            encode(Key, In2, Config, Out);
        Other -> 
            Other
    end;

encode(#{ key := Key, in := At }, In, Config, Out) when is_atom(Key) and is_map(At) -> 
    case encode(At, In, Config, Out) of 
        {ok, In2} ->
            encode(Key, In2, Config, Out);
        Other -> 
            Other
    end;

encode(#{ key := Key}, In, Config, Out) when is_atom(Key) -> 
    encode(Key, In, Config, Out);

encode(#{ type := text,
          value := Value }, _, _, _) ->
    {ok, cmkit:to_bin(Value) };


encode(#{ type := number,
          value:= V }, _, _, _) when is_number(V) ->
    {ok, V};

encode(#{ type := member,
          spec := #{ value := ValueSpec,
                     in := CollectionSpec }}, In, Config, _) ->
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
          spec := Spec }, In, Config, Out) ->
    encode(Spec, In, Config, Out);

encode(#{ type := keyword,
          value := Value }, _, _, _) when is_atom(Value) ->
    {ok, Value};

encode(#{ type := object,
          spec := Spec }, In, Config, Out) ->
    encode(maps:keys(Spec), Spec, In, Config, Out);

encode(#{ type := list,
          value := List }, In, Config, Out) when is_list(List)->
    {ok, lists:map(fun(V) ->
                           {ok, V2} = encode(V, In, Config, Out),
                           V2 
                   end, List)};

encode(#{ type := config,
          spec := Key }, _, Config, _) ->
    {ok, maps:get(Key, Config)};


encode(#{ type := request,
         spec := Spec }, In, Config, Out) ->
    encode(Spec, In, Config, Out);

encode(#{ type := http,
          method := Method,
          body := Body,
          headers := Headers }, In, Config, Out) ->
    case encode(Headers, In, Config, Out) of 
        {ok, H} ->
            case encode(Body, In, Config, Out) of 
                {ok, B} ->
                    {ok, #{ method => Method,
                            headers => H,
                            body => B }}; 
                Other -> Other
            end;
        Other -> Other
    end;

encode(#{ type := path,
          location := Path }, _, _, _) -> {ok, cmkit:to_list(Path)};

encode(#{ type := file,
          spec := Spec }, In, Config, Out) ->
    case encode(Spec, In, Config, Out) of 
        {ok, Path} ->
            file:read_file(Path);
        Other -> 
            Other
    end;


encode(#{ type := greater_than,
          spec := [Spec1, Spec2] }, In, Config, Out) ->
    case { encode(Spec1, In, Config, Out), encode(Spec2, In, Config, Out) } of 
        { {ok, V1}, {ok, V2} } when (is_number(V1) and is_number(V2)) ->
            {ok, V1 > V2};
        Other -> Other
    end;

encode(#{ type := sum,
          spec := Specs }, In, Config, Out) ->
    Res = lists:foldl(fun({ok, V}, Total) when is_number(V) -> 
                        Total + V;
                   (Other, _) -> 
                        {error, {not_a_number, Other}}
                end, 0, lists:map(fun(S) ->
                                       encode(S, In, Config, Out)
                               end, Specs)),
    case Res of 
        N when is_number(N) -> {ok, N};
        Other -> Other
    end;

encode(#{ spec := Spec }, _, _, _) -> {ok, Spec}.

encode([], _, _, _, Out) -> {ok, Out};
encode([K|Rem], Spec, In, Config, Out) ->
    case encode(maps:get(K, Spec), In, Config, Out) of 
        {ok, V} ->
            encode(Rem, Spec, In, Config, Out#{ K => V });
        Other -> Other
    end.
