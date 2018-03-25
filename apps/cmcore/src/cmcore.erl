-module(cmcore).
-export([init/2, update/2]).

init(Data, Session) ->
    {ok, Context} = cmcore_sup:start_context(Session),
    
    cmkit:log({cmcore, context, Context}),
    gen_statem:cast(Context, {init, Data}).

update(Id, Data) ->
    case cmcore_util:context(Id) of 
        {ok, Context} ->
            gen_statem:cast(Context, {update, Data});
        Other ->
            cmkit:log({core, update, Id, Other})
    end.
