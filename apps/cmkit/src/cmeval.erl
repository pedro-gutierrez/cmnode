-module(cmeval).
-export([eval/2]).

eval(#{ type := true }, _) -> true;
eval(#{ type := false }, _) -> false;

eval(#{ type := equal, 
        spec := Spec }, In) -> 
    case cmdecode:decode(#{ type => object,
                            spec => Spec }, In) of 
        {ok, _} -> true;
        _ -> false
    end;

eval(#{ type := member, 
        spec := Spec }, In) -> 
    case lists_members_specs(Spec, In) of 
        {ok, Spec2} -> 
            cmkit:log({cmeval, member, #{ type => object,
                                    spec => Spec2 }, In}),
            case cmdecode:decode(#{ type => object,
                                    spec => Spec2 }, In) of
                {ok, _} -> true;
                _ -> false
            end;
        _ -> false
    end;

eval(#{ type := present, spec := Keys}, In) when is_map(In) ->
    cmkit:has_all_keys(Keys, In);

eval(_, _) -> false.

lists_members_specs(Spec, In) ->
    lists_members_specs(maps:keys(Spec), Spec, In, #{}).

lists_members_specs([], _, _, Out) -> {ok, Out};
lists_members_specs([K|Rem], Spec, In, Out) ->
    ValueSpec = maps:get(K, Spec),
    case cmencode:encode(ValueSpec) of 
        {ok, Value} ->
            lists_members_specs(Rem, Spec, In,
                                maps:put(K, #{ type => list,
                                               with => Value }, Out));
        Other -> Other
    end.

