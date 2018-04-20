-module(cmeval).
-export([eval/2]).

eval(#{ op := true }, _) -> true;
eval(#{ op := present, params := Keys}, In) when is_map(In) ->
    cmkit:has_all_keys(Keys, In);

eval(_, _) -> false.
