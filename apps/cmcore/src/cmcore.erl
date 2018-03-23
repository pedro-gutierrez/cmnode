-module(cmcore).
-export([init/2]).

init(Data, Session) ->
    {ok, Context} = cmcore_sup:start_context(Session),
    
    cmkit:log({cmcore, context, Context}),
    ok = gen_statem:call(Context, {init, Data}).
