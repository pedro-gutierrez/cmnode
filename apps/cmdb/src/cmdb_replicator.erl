-module(cmdb_replicator).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(#{ name := Name }=Bucket) ->
    RegName = cmdb_config:replicator(Name),
    gen_server:start_link({local, RegName},?MODULE, [Bucket], []).

init([Bucket]) ->
    {ok, Bucket}.

handle_call(_, _, Data) ->
    {reply, Data, ok}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #{ name := Name}) ->
    cmkit:log({cmdb, replicator, Name, node(), Reason}),
    ok.
