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
                {ok, #{ contact := []}} ->
                    Log = cmkit:log_fun(false),
                    State = set_state(offline, Log),
                    {ok, State, #{ log => Log}};

                {ok, #{ name := Name,
                        size := Size,
                        contact := Nodes } = Spec0} ->

                    Log = cmkit:log_fun(Spec0),
                    Log({cmcluster, monitoring}),
                    global_group:monitor_nodes(true),
                    Log({cmcluster, topic}),
                    Cluster = {cluster, cmkit:to_atom(Name)},
                    cmbus:create_sub(Cluster),
                    NodeNames = cmkit:node_names(Nodes, cmkit:host()),
                    Log({cmcluster, pinging, NodeNames}),
                    Pings = cmcluster_util:ping(NodeNames, node()),
                    Log({cmcluster, [P||P<-Pings, P =/= self]}),
                    State = set_state(connecting, Log),
                    {ok, State, #{ cluster => Cluster,
                                   size => Size,
                                   up => 0,
                                   log => Log,
                                   contact => Nodes}};
                {ok, Spec0} ->
                    Log = cmkit:log_fun(Spec0),
                    State = set_state(offline, Log),
                    {ok, State, #{ log => Log}}
            end;
        {error, not_found} ->
            Log = cmkit:log_fun(false),
            State = set_state(offline, Log),
            {ok, State, #{ log => Log}}
    end.

offline(info, Msg, Data) ->
    cmkit:log({cmcluster, info, Msg}),
    {keep_state, Data}.

online(info, {nodeup, N}, Data) ->
    {State, Data2} = node_up(N, Data),
    {next_state, State, Data2};

online(info, {nodedown, N}, Data) ->
    {State, Data2} = node_down(N, Data),
    {next_state, State, Data2};

online(info, Msg, Data) ->
    cmkit:log({cmcluster, info, Msg}),
    {keep_state, Data}.

connecting(info, {nodeup, N}, Data) ->
    {State, Data2} = node_up(N, Data),
    {next_state, State, Data2};

connecting(info, {nodedown, N}, Data) ->
    {State, Data2} = node_down(N, Data),
    {next_state, State, Data2};

connecting(info, Msg, Data) ->
    cmkit:log({cmcluster, info, Msg}),
    {keep_state, Data}.

terminate(Reason, _, _) ->
    cmkit:log({cmcluster, terminate, Reason}),
    ok.

node_down(N, #{ size := Size, 
                up := Up0,
                log := Log} = Data) ->
    Up = Up0 -1,
    Log({cmcluster, down, N}),
    State = set_state(state(Size, Up), Log),
    {State, Data#{ up => Up }}.

node_up(N, #{ size := Size, 
              up := Up0,
              log := Log} = Data) ->
    Up = Up0 +1,
    State = set_state(state(Size, Up), Log),
    Log({cmcluster, up, N}),
    cmkit:set_app_env(cmcluster, cluster, State),
    {State, Data#{ up => Up }}.

set_state(State, Log) ->
    cmkit:set_app_env(cmcluster, state, State),
    Log({cmcluster, State}),
    State.

state(Size, Up) when Up + 1 =:= Size -> online;
state(_, _) -> connecting.

