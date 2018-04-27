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
           {error, #{ status => missing_key,
                      key => Key,
                      data => In }};
       V ->
           {ok, V}
   end;

encode(#{ from := Key, at := At }, In, Config, Out) when is_atom(Key) and ( is_atom(At) or is_binary(At))-> 
    case encode(At, In, Config, Out) of 
        {ok, In2} ->
            encode(Key, In2, Config, Out);
        Other -> 
            Other
    end;

encode(#{ from := Key, at := At }, In, Config, Out) when is_atom(Key) and is_map(At) -> 
    case encode(At, In, Config, Out) of 
        {ok, In2} ->
            encode(Key, In2, Config, Out);
        Other -> 
            Other
    end;

encode(#{ from := Key}, In, Config, Out) when is_atom(Key) -> 
    encode(Key, In, Config, Out);

encode(#{ type := text,
          value := Value }, _, _, _) ->
    {ok, cmkit:to_bin(Value) };

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

encode(#{ spec := Spec }, _, _, _) -> {ok, Spec}.

encode([], _, _, _, Out) -> {ok, Out};
encode([K|Rem], Spec, In, Config, Out) ->
    case encode(maps:get(K, Spec), In, Config, Out) of 
        {ok, V} ->
            encode(Rem, Spec, In, Config, Out#{ K => V });
        Other -> Other
    end.
