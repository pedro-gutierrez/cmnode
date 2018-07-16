-module(cmdb_directory).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    State = cmcloud:state(),
    cmkit:log({cmdb, node(), State}),
    {ok, State}.

handle_call(_, _, Data) ->
    {reply, Data, Data}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info({nodedown, Node}, Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, node(), State, nodedown, Node}),
    {noreply, Data};

handle_info({nodeup, Node}, Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, node(), State, nodeup, Node}),
    {noreply, Data};

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _) ->
    cmkit:log({cmdb_directory, node(), terminated, Reason}),
    ok.
