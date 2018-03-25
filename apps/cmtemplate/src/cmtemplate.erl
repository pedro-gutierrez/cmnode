-module(cmtemplate).
-export([render/2, reload/0]).

render(T, Data) ->
    case cmtemplate_util:is_template(T) of 
        true -> 
            cmkit:log({cmtemplate, render, T, Data}),
            case T:render(maps:to_list(Data)) of 
                {ok, List} ->
                    {ok, cmkit:bin_join(List, <<"">>)};
                Other  ->
                    cmkit:log({template, T, Other}),
                    Other
            end;
        false ->
            cmkit:log({template, T, not_found}),
            {error, not_found}
    end.

reload() ->
    cmtemplate_util:reload().
