-module(cmcore).
-export([init/3, update/2]).

init(Data, Spec, #{ app := _, id := _}=Session) ->
    {ok, Context} = cmcore_sup:start_context(Spec, Session),
    gen_statem:cast(Context, {init, Data}).

update(Id, Data) when is_binary(Id) ->
    case cmcore_util:context(Id) of 
        {ok, Context} ->
            gen_statem:cast(Context, {update, Data});
        Other ->
            cmkit:log({cmcore, update, Id, context_error, Other})
    end.
