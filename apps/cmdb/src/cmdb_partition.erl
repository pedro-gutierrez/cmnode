-module(cmdb_partition).
-behaviour(gen_server).
-export([start_link/1]).
-export([reset/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

reset(Partition) -> 
    RegName = cmdb_util:partition_name(Partition),
    gen_server:call(RegName, reset).

start_link(#{ partition := Partition }=Bucket) ->
    RegName = cmdb_util:partition_name(Partition),
    gen_server:start_link({local, RegName}, ?MODULE, [Bucket], []).

init([Partition]) ->
    case cmdb_util:open_partition(Partition) of 
        {ok, Tid} -> 
            State = Partition#{ tid => Tid },
            cmdb_bucket:register_partition(State),
            {ok, State};
        {error, E} ->
            cmkit:danger({cmdb_partition, Partition, failed, E}),
            {stop, Partition#{ error => E }}
    end.

handle_call(reset, _, #{ name := Name,
                         tid := Tid }=Bucket) ->
    Res = cmdb_util:reset_partition(Name, Tid),
    {reply, Res, Bucket};

handle_call({put, Pairs, []}, _, #{ name := Name,
                                                tid := Tid }=Bucket) ->

    Res = cmdb_util:put(Name, Tid, Pairs),
    {reply, Res, Bucket};

handle_call({put, Pairs, [{replicas, N}]}, _, #{ name := Name,
                                                tid := Tid }=Bucket) ->

    Res = cmdb_util:put(Name, Tid, Pairs),
    Nodes = lists:sublist(nodes(), N),
    cmkit:cast(Nodes, cmdb, put, [Name, Pairs, []]),
    {reply, Res, Bucket};

handle_call({put_new, Pairs, []}, _, #{ name := Name,
                                                     tid := Tid}=Bucket) ->
    Res = cmdb_util:put_new(Name, Tid, Pairs),
    {reply, Res, Bucket};

handle_call({put_new, Pairs, [{replicas, N}]}, _, #{ name := Name,
                                                     tid := Tid}=Bucket) ->
    Res = cmdb_util:put_new(Name, Tid, Pairs),
    Nodes = lists:sublist(nodes(), N),
    cmkit:cast(Nodes, cmdb, put_new, [Name, Pairs, []]),
    {reply, Res, Bucket}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #{ name := Name}) ->
    cmkit:log({cmdb, replicator, Name, node(), Reason}),
    ok.
