-module(cmdb_cloud).
-behaviour(gen_statem).
-export([
         status/0,
         node_for/1,
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).
-record(data, {dbs, index}).

status() -> 
    gen_statem:call({cmdb_cloud, node()}, ping).

node_for(Db) -> 
    gen_statem:call({cmdb_cloud, node()}, {node, Db}).

callback_mode() ->
    state_functions.

start_link(Dbs) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Dbs], []).

init([Dbs]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    cmkit:log({cmdb, cmcloud:state(), node()}),
    {ok, ready, #data{dbs=Dbs, index=index(Dbs)}}.

ready(info, {nodedown, Node}, #data{index=Index}=Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, State, nodedown, Node}),
    {next_state, ready, Data#data{index=refresh(Index)}};

ready(info, {nodeup, Node}, #data{index=Index}=Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, State, nodeup, Node}),
    {next_state, ready, Data#data{index=refresh(Index)}};

ready(info, Msg, Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, State, message, Msg}),
    {next_state, ready, Data};

ready({call, From}, ping, #data{index=I}=Data) ->
    Res = {ok, I}, 
    {keep_state, Data, {reply, From, Res}};



ready({call, From}, {node, Name}, #data{index=I}=Data) ->
    Res = case I of 
              #{ Name := {_, none, _}} -> 
                  {error, no_enough_nodes};
              #{ Name := {_, N, _}} -> 
                  {ok, N};
              _ -> 
                  {error, no_such_db}
          end,
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, _) ->
    cmkit:log({cmdb, terminate, Reason}),
    ok.

index(Dbs) -> 
    refresh(lists:foldl( fun(#{ name := Name, hosts := Hosts}, Index) ->
                                 {Nodes, _} = cmkit:hosts_to_nodes(Hosts),
                                 Fav = fav(Nodes),
                                 maps:put( Name, {Hosts, Fav, Nodes}, Index)
               end, #{}, Dbs)).

refresh(Index) ->
    maps:fold(fun(Name, {Hosts, _, _}, I) ->
                      {Nodes, _} = cmkit:hosts_to_nodes(Hosts),
                      Fav = fav(Nodes),
                      maps:put(Name, {Hosts, Fav, Nodes}, I) 
              end, Index, Index).

fav([N|_]) -> N;
fav([]) -> none.
