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

eval(#{ type := present, spec := Keys}, In) when is_map(In) ->
    cmkit:has_all_keys(Keys, In);

eval(_, _) -> false.
