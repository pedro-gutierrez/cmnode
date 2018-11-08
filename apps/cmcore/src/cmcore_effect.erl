-module(cmcore_effect).
-behaviour(gen_server).
-export([start_link/1, apply/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

stop(Pid) ->
    case erlang:is_process_alive(Pid) of 
        true -> 
            gen_server:cast(Pid, stop);
        false ->
            ok
    end.

apply(Pid, Mod, Data) ->
    gen_server:cast(Pid, {apply, Mod, Data}).

start_link(SessionId) ->
    gen_server:start_link(?MODULE, [SessionId], []).

init([SessionId]) ->
    {ok, SessionId}.

handle_call(Msg, _, SessionId) ->
    cmkit:warning({cmeffect, SessionId, ignored, Msg}),
    {reply, ignored, SessionId}.

handle_cast(stop, SessionId) ->
    cmkit:log({cmeffect, SessionId, terminating}),
    {stop, normal, SessionId};

handle_cast(Msg, SessionId) ->
    handle(Msg, SessionId),
    {noreply, SessionId}.

handle_info(Msg, SessionId) ->
    handle(Msg, SessionId),
    {noreply, SessionId}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, SessionId) ->
    cmkit:log({cmeffect, SessionId, terminated}),
    ok.

handle({apply, Mod, Data}, SessionId) ->
    Mod:effect_apply(Data, SessionId);

handle(Other, SessionId) ->
    cmkit:warning({cmeffect, SessionId, ignored, Other}),
    ok.
