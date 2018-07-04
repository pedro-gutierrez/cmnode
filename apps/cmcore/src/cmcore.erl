-module(cmcore).
-export([init/2, terminate/1, update/2]).

init(Spec, #{ app := _, id := _}=Session) ->
    {ok, Context} = cmcore_context_sup:start_context(Spec, Session),
    gen_statem:cast(Context, init). 

update(Id, Data) when is_binary(Id) ->
    case cmcore_util:context(Id) of 
        {ok, Context} ->
            gen_statem:cast(Context, {update, Data});
        Other ->
            cmkit:log({cmcore, error, Id, no_such_context, Other})
    end.

terminate(Id) when is_binary(Id) -> 
    case cmcore_util:context(Id) of 
        {ok, Pid} ->
            gen_statem:cast(Pid, terminate);
        Other ->
            cmkit:log({cmcore, error, Id, no_such_context, Other})
    end.
