-module(cmeval).
-export([eval/3]).

eval(#{ type := true }, _, _) -> true;
eval(#{ type := false }, _, _) -> false;

eval(#{ type := equal, 
        spec := Specs }, In, Config) when is_list(Specs) -> 
    all_equal(lists:map(fun(Spec) ->
                                cmencode:encode(Spec, In, Config)
                        end, Specs));

eval(#{ type := member, 
        spec := Spec }, In, Config) -> 
    case lists_members_specs(Spec, In) of 
        {ok, Spec2} -> 
            case cmdecode:decode(#{ type => object,
                                    spec => Spec2 }, In, Config) of
                {ok, _} -> true;
                _ -> false
            end;
        _ -> false
    end;

eval(#{ type := present, spec := Keys}, In, _) when is_map(In) ->
    cmkit:has_all_keys(Keys, In);

eval(_, _, _) -> false.

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

all_equal([V|Rem]) -> all_equal(Rem, V).
all_equal([], _) -> true;
all_equal([V|Rem], V) -> all_equal(Rem, V);
all_equal(_, _) -> false.


