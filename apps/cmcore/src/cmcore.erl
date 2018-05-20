-module(cmcore).
-export([init/2, update/2]).

init(Spec, #{ app := _, id := _}=Session) ->
    {ok, Context} = cmcore_sup:start_context(Spec, Session),
    gen_statem:cast(Context, init). 

update(Id, Data) when is_binary(Id) ->
    case cmcore_util:context(Id) of 
        {ok, Context} ->
            gen_statem:cast(Context, {update, Data});
        Other ->
            cmkit:log({cmcore, error, Id, no_context, Other})
    end.
