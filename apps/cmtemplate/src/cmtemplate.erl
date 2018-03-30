-module(cmtemplate).
-export([render/2, reload/0]).

render(T, Data) ->
    case cmtemplate_util:is_template(T) of 
        true -> 
            case T:render(Data) of 
                {ok, List} ->
                    {ok, cmkit:bin_join(List)};
                Other  ->
                    Other
            end;
        false ->
            {error, not_found}
    end.

reload() ->
    cmtemplate_util:reload().
