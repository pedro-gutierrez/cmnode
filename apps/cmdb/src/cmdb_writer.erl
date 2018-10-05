-module(cmdb_writer).
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

start_link(#{ name := Name }=Bucket) ->
    Writer = cmdb_config:writer(Name),
    gen_server:start_link({local, Writer}, ?MODULE, [Bucket], []).

init([#{ name := Name }=Bucket]) ->
    Storage = cmdb_config:storage(Name),
    {ok, Pid} = cmdb_util:open(Storage, Name),
    Ref = erlang:monitor(process, Pid),
    {ok, Header, _} = cbt_file:read_header(Pid),
    {_, Root} = Header,
    {ok, Tree} = cbt_btree:open(Root, Pid),
    cmkit:log({cmdb, writer, self(), Name, Storage, Pid, node()}),
    {ok, Bucket#{ tree => Tree, pid => Pid, ref => Ref }}.

handle_call(reset, _, #{ name := Name,
                         pid := Fd,
                         ref := Ref }=Data) ->
    erlang:demonitor(Ref),
    ok = cbt_file:close(Fd),
    Storage = cmdb_config:storage(Name),
    cmdb_util:delete(Storage, Name),
    {ok, Data2} = init([Data]),
    {reply, ok, Data2};

handle_call({put, Entries}, _, #{ pid := Pid, tree := Tree }=Data) ->
    {ok, Tree2} = write_entries(Pid, Tree, Entries),
    {reply, ok, Data#{ tree => Tree2}};

handle_call({put, S, Match, Merge}, _, #{ pid := Pid, tree := Tree }=Data) ->
    case cmdb_util:get(disc, Pid, S) of 
        {ok, Entries} ->
            ValueSpec = #{ type => object,
                           spec => Match },

            Now = cmkit:micros(),
            Host = cmdb_config:host(),
            ToUpdate = lists:foldr(fun({S0, P0, O0, V}, Acc) when is_map(V) ->
                                           case cmdecode:decode(ValueSpec, V) of 
                                               {ok, _} ->
                                                   [{{S0, P0, O0, Host, Now},
                                                     maps:merge(V, Merge)}|Acc];
                                               _ -> 
                                                   Acc
                                           end;
                                      (_, Acc) ->
                                           Acc
                                   end, [], cmdb_util:merge(Entries)),
            case ToUpdate of 
                [] -> 
                    {reply, ok, Data};
                _ -> 
                    {ok, Tree2} = write_entries(Pid, Tree, ToUpdate),
                    {reply, ok, Data#{ tree => Tree2}}
            end;
        Other ->
            {reply, Other, Data}
    end;

handle_call(_, _, Data) ->
    {reply, ignored, Data}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, Bucket) ->
    cmkit:warning({cmdb, writer, node(), Bucket, terminated, Reason}),
    ok.

write_entries(Pid, Tree, Entries) -> 
    {ok, Tree2} = cbt_btree:add(Tree, Entries),
    Root2 = cbt_btree:get_state(Tree2),
    Header2 = {1, Root2},
    cbt_file:write_header(Pid, Header2),
    {ok, Tree2}.
