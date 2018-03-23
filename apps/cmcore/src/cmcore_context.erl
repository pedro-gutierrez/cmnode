-module(cmcore_context).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         initializing/3
        ]).

callback_mode() ->
    state_functions.

start_link(Session) ->
    gen_statem:start_link(?MODULE, [Session], []).

init([#{ id := Id, app := App }=Session]) ->
    ok = cmsession:attach(Id, context, self()),
    cmkit:log({cmcore, init, App, Id}),
    {ok, initializing, Session}.

initializing({call, From}, {init, Data},  #{id := SessionId}=State) ->
    cmkit:log({cmcore, init, Data}),
    cmsession:tell(SessionId, #{ msg => hello }),
    {keep_state, State, {reply, From, ok}}.

terminate(Reason, _, #{ app := App, id := Id}) ->
    cmkit:log({cmcore, App, Id, node(), terminated, Reason}),
    ok.
