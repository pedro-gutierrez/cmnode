-module(cmdb_replicator).
-behaviour(gen_server).
-export([
         replicate/3,
         start_link/1
        ]).
-export([
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
        ]).

start_link(#{ name := Name }=Bucket) ->
    Replicator = cmdb_config:replicator(Name),
    gen_server:start_link({local, Replicator}, ?MODULE, [Bucket], []).

replicate(Name, out, Entries) ->
    gen_server:call(Name, {replicate, Entries});

replicate(Name, in, Entries) ->
    gen_server:cast(Name, {replicate, Entries}).

init([#{ name := Name,
         debug := Debug }=Bucket]) ->

    Log = cmkit:log_fun(Debug),
    Topic = {replicas, Name},
    cmbus:create_sub(Topic),
    cmkit:log({cmdb,  Name, replicator}),
    {ok, Bucket#{ host => node(),
                  writer => cmdb_config:writer(Name),
                  topic => Topic,
                  log => Log }}.


handle_call({replicate, Entries}, _, #{ log := Log,
                                        topic := Topic,
                                        name := Name}=Data) ->

    Peers = cmbus:peers(Topic),
    lists:foreach(fun(Pid) ->
                          replicate(Pid, in, Entries)
                  end, Peers),
    Log({Name, self(), replicate, Entries, Peers}),
    {reply, ok, Data};

handle_call(Msg, _, #{ log := Log,
                       name := Name}=Data) ->

    Log({Name, ignored, Msg}),
    {reply, ok, Data}.

handle_cast({replicate, Entries}, #{ name := Name,
                                     log := Log,
                                     writer := Writer }=Data) ->
    
    Res = gen_server:call(Writer, {replicate, Entries}),
    Log({Name, replicate, in, Entries, Res}),
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #{ name := Name }) ->
    cmkit:warning({cmdb, Name, replicator, terminated, Reason}),
    ok.
