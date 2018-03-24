-module(cmeffect_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(#{ name := Name}=Effect) ->
    gen_server:start_link({local, Name}, ?MODULE, [Effect], []).

init([#{ name := Name }=Effect]) ->
    cmkit:log({effect, Name, ok}), 
    {ok, Effect}.

handle_call(Msg, _, Effect) ->
    apply_effect(Msg, Effect),
    {reply, ok, Effect}.

handle_cast(Msg, Effect) ->
    apply_effect(Msg, Effect),
    {noreply, Effect}.

handle_info(Msg, Effect) ->
    apply_effect(Msg, Effect),
    {noreply, Effect}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #{ name := Name }) ->
    cmkit:log({effect, Name, terminated, Reason}),
    ok.

apply_effect({apply, Data, Session}, #{ mod := Mod}) ->
    Mod:effect_apply(Data, Session).
