-module(cmdb2_writer).
-behaviour(gen_server).
-export([
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

start_link(#{ writer := Writer }=Bucket) ->
    gen_server:start_link({local, Writer}, ?MODULE, [Bucket], []).

init([#{ name := Name }=Bucket]) ->
    Storage = cmdb2_config:storage(Name),
    {ok, Pid} = cmdb2_util:open(Storage, Name),
    Ref = erlang:monitor(process, Pid),
    cmkit:log({cmdb, writer, Name, Storage, node()}),
    {ok, Bucket#{ fd => #{ pid => Pid, ref => Ref }}}.

handle_call(close, _, #{ name := Name,
                         fd := #{ pid := Fd,
                                  ref := Ref }}=Data) ->
    erlang:demonitor(Ref),
    ok = cbt_file:close(Fd),
    Storage = cmdb2_config:storage(Name),
    cmdb2_util:delete(Storage, Name),
    {ok, Data2} = init([Data]),
    {reply, ok, Data2};

handle_call({put, Entries}, _, #{ fd := #{ pid := Fd }}=Data) ->
    {ok, Header, _} = cbt_file:read_header(Fd),
    {_, Root} = Header,
    {ok, Tree} = cbt_btree:open(Root, Fd),
    {ok, Tree2} = cbt_btree:add(Tree, Entries),
    Root2 = cbt_btree:get_state(Tree2),
    Header2 = {1, Root2},
    cbt_file:write_header(Fd, Header2),
    {reply, ok, Data};

handle_call(_, _, Data) ->
    {reply, Data, Data}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, Bucket) ->
    cmkit:warning({cmdb, writer, node(), Bucket, terminated, Reason}),
    ok.
