-module(cmcluster_watcher).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         offline/3,
         online/3,
         connecting/3
        ]).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case cmconfig:settings(cluster) of 
        {ok, #{ spec := Spec }} ->
            case cmencode:encode(Spec) of 
                {ok, #{ name := Name,
                        size := Size,
                        contact := Nodes }} ->
                    global_group:monitor_nodes(true),
                    ClusterName = {cluster, cmkit:to_atom(Name)},
                    cmbus:create(ClusterName),
                    cmbus:sub(ClusterName),
                    NodeNames = cmkit:node_names(Nodes, cmkit:host()),
                    cmcluster_util:ping(NodeNames, node()),
                    cmkit:log({cmcluster, node(), connecting}),
                    {ok, connecting, #{ cluster => ClusterName,
                                        size => Size,
                                        up => 0,
                                        contact => Nodes}};
                {ok, _} ->
                    cmkit:log({cmcluster, node(), offline}),
                    {ok, offline, #{}}
            end;
        {error, not_found} ->
            cmkit:log({cmcluster, node(), offline}),
            {ok, offline, #{}}
    end.

offline(info, Msg, Data) ->
    cmkit:log({cmcluster, node(), info, Msg}),
    {keep_state, Data}.

online(info, {nodeup, N}, Data) ->
    {State, Data2} = node_up(N, Data),
    {next_state, State, Data2};

online(info, {nodedown, N}, Data) ->
    {State, Data2} = node_down(N, Data),
    {next_state, State, Data2};

online(info, Msg, Data) ->
    cmkit:log({cmcluster, node(), info, Msg}),
    {keep_state, Data}.

connecting(info, {nodeup, N}, Data) ->
    {State, Data2} = node_up(N, Data),
    {next_state, State, Data2};

connecting(info, {nodedown, N}, Data) ->
    {State, Data2} = node_down(N, Data),
    {next_state, State, Data2};

connecting(info, Msg, Data) ->
    cmkit:log({cmcluster, node(), info, Msg}),
    {keep_state, Data}.

terminate(Reason, _, _) ->
    cmkit:log({cmcluster, terminate, Reason}),
    ok.

node_down(N, #{ size := Size, up := Up0} = Data) ->
    Up = Up0 -1,
    State = state(Size, Up),
    cmkit:warning({cmcluster, State, down, N}),
    {State, Data#{ up => Up }}.

node_up(N, #{ size := Size, up := Up0} = Data) ->
    Up = Up0 +1,
    State = state(Size, Up),
    cmkit:success({cmcluster, State, up, N}),
    {State, Data#{ up => Up }}.

state(Size, Up) when Up + 1 =:= Size -> online;
state(_, _) -> connecting.

