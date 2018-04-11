-module(cmcore).
-export([init/2, update/2]).

init(Data, #{ app := _, id := _}=Session) ->
    {ok, Context} = cmcore_sup:start_context(Session),
    
    cmkit:log({cmcore, context, Context}),
    gen_statem:cast(Context, {init, Data}).

update(Id, Data) when is_binary(Id) ->
    case cmcore_util:context(Id) of 
        {ok, Context} ->
            gen_statem:cast(Context, {update, Data});
        Other ->
            cmkit:log({cmcore, update, Id, context_error, Other})
    end.
