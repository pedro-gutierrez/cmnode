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

init([#{ name := Name,
         debug := Debug }=B]) ->

    Log = cmkit:log_fun(Debug),
    Storage = cmdb_config:storage(Name),
    {ok, Pid} = cmdb_util:open(Storage, Name),
    Ref = erlang:monitor(process, Pid),
    {ok, Header, _} = cbt_file:read_header(Pid),
    {_, Root} = Header,
    {ok, Tree} = cbt_btree:open(Root, Pid),
    Log({cmdb,  Name, Storage}),
    B2 = B#{ host => node(),
             timestamp_fun => timestamp_fun(B),
             log => Log, 
             tree => Tree, 
             pid => Pid, 
             ref => Ref },
    {ok, with_replicator(B2)}.

with_replicator(#{ cluster := offline } = B) ->
    B;

with_replicator(#{ name := Name }=B) ->
    B#{ replicator => cmdb_config:replicator(Name)}.

handle_call(reset, _, Data) ->
    case replicate(reset, Data) of 
        ok ->
            {ok, Data2} = reset(Data),
            {reply, ok, Data2};
        Other ->
            {reply, Other, Data}
    end;

handle_call({local, reset}, _, Data) ->
    
    {ok, Data2} = reset(Data),
    {reply, ok, Data2};

handle_call({replicate, Entries}, _, #{ pid := Pid, 
                                        tree := Tree }=Data) ->
    case write_entries(Pid, Tree, Entries) of 
        {ok, Tree2} -> 
            {reply, ok, Data#{ tree => Tree2}};
        Other ->
            {reply, Other, Data}
    end;


handle_call({put, ToAdd}, _, #{ host := Host, 
                                pid := Pid, 
                                tree := Tree,
                                timestamp_fun := TFun }=Data) ->
    
    ToAdd2 = unique_entries(ToAdd, Host, TFun()),
    case with_entries(Tree, ToAdd2) of 
        {ok, Tree3} ->
            write_and_reply(Tree3, Pid, Data);
        Other ->
            {reply, Other, Data}
    end;


handle_call({put, S, Match, Merge}, _, #{ pid := Pid, 
                                          tree := Tree }=Data) ->
    case cmdb_util:get(disc, Tree, S) of 
        {ok, Entries} ->
            case map_entries(Pid, Tree, Entries, Match, Merge, Data) of 
                {ok, Tree2} ->
                    {reply, ok, Data#{ tree => Tree2}};
                Other ->
                    {reply, Other, Data}
            end;
        Other ->
            {reply, Other, Data}
    end;

handle_call({put, S, P, Match, Merge}, _, #{ pid := Pid, 
                                             tree := Tree }=Data) ->
    case cmdb_util:get(disc, Tree, S, P) of 
        {ok, Entries} ->
            case map_entries(Pid, Tree, Entries, Match, Merge, Data) of 
                {ok, Tree2} ->
                    {reply, ok, Data#{ tree => Tree2}};
                Other ->
                    {reply, Other, Data}
            end;
        Other ->
            {reply, Other, Data}
    end;


handle_call({delete, Keys}, _, #{ pid := Pid,
                                  tree := Tree }=Data) ->

    case without_keys(Tree, Keys) of 
        {ok, Tree2} ->
            write_and_reply(Tree2, Pid, Data);
        Other ->
            {reply, Other, Data}
    end;

handle_call({put_delete, ToAdd, ToDelete}, _, #{ pid := Pid,
                                                 host := Host,
                                                 timestamp_fun := TFun,
                                                 tree := Tree }=Data) when is_list(ToAdd) andalso is_list(ToDelete) ->

    case without_keys(Tree, ToDelete) of 
        {ok, Tree2} ->
            ToAdd2 = unique_entries(ToAdd, Host, TFun()),
            case with_entries(Tree2, ToAdd2) of 
                {ok, Tree3} ->
                    write_and_reply(Tree3, Pid, Data);
                Other ->
                    {reply, Other, Data}
            end;
        Other ->
            {reply, Other, Data}
    end;


handle_call({insert, ToInsert}, _, #{ pid := Pid,
                                      host := Host,
                                      timestamp_fun := TFun,
                                      tree := Tree }=Data) when is_list(ToInsert) ->
    case all_new(Tree, ToInsert) of 
        true ->
            ToInsert2 = unique_entries(ToInsert, Host, TFun()),
            case with_entries(Tree, ToInsert2) of
                {ok, Tree2} ->
                    write_and_reply(Tree2, Pid, Data);
                Other ->
                    {reply, Other, Data}
            end;
        false ->
            {reply, {error, conflict}, Data};
        Other ->
            {reply, Other, Data}
    end;

handle_call({pipeline, #{ context := Context,
                          items := #{ type := list,
                                      value := Items}}}, _, #{ pid := Pid, 
                                                               tree := Tree }=Data) ->
    
    case pipeline(Tree, Context, Items, Data, []) of 
        {ok, Entries} ->
            case write_entries(Pid, Tree, Entries, Data) of 
                {ok, Tree2} ->
                    {reply, ok, Data#{ tree => Tree2}};
                Other ->
                    {reply, Other, Data}
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

reset(#{ log := Log,
         name := Name,
         pid := Fd,
         ref := Ref} = Data) ->
    erlang:demonitor(Ref),
    ok = cbt_file:close(Fd),
    Storage = cmdb_config:storage(Name),
    cmdb_util:delete(Storage, Name),
    {ok, Data2} = init([Data]),
    Log({Name, resetted}),
    {ok, Data2}.

all_new(_, []) -> true;
all_new(Tree, [{S, P, O, _}|Rem]) ->
    case cmdb_util:keys_for({S, P, O}, Tree) of 
        {ok, []} ->
            all_new(Tree, Rem);
        {ok, _} ->
            false;
        Other ->
            Other
    end.

without_keys(Tree, Keys) ->
    cmdb_util:reduce(Tree,
                     fun cmdb_util:keys_for/2,
                     fun cmdb_util:without_keys/2, Keys).


with_entries(Tree, Entries) ->
   cbt_btree:add(Tree, Entries).

write_and_reply(Tree, Pid, Data) ->
    case write_tree(Pid, Tree) of 
        {ok, Tree2} ->
            {reply, ok, Data#{ tree => Tree2}};
        Other ->
            {reply, Other, Data}
    end.

unique_entries(E, H, T) ->
    maps:to_list(lists:foldl(fun({S, P, O, V}, Idx) ->
                                 Idx#{ {S, P, O, H, T} => V};
                            (_, Idx) -> 
                                 Idx
                         end, #{}, E)).


map_entries(Pid, Tree, Entries, Match, Merge, #{ name := Name,
                                                 log := Log,
                                                 host := Host,
                                                 timestamp_fun := TFun } = Data) ->

    ValueSpec = #{ type => object,
                   spec => Match },

    Now = TFun(),
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
                           end, [], Entries),
    Log({Name, map, Match, Merge, Entries, ToUpdate}),
    case ToUpdate of 
        [] -> 
            {ok, Tree};
        _ -> 
            write_entries(Pid, Tree, ToUpdate, Data)
    end.


%timestamp_fun(#{ replication := none }) ->
%    fun() -> 0 end;

timestamp_fun(_) -> 
    fun() -> cmkit:micros() end.


write_entries(Pid, Tree, Entries, #{ name := Name, 
                                     log := Log }=B) -> 
    T0 = cmkit:micros(),
    {ok, Tree2} = write_entries(Pid, Tree, Entries),
    replicate(Entries, B),
    Log({Name, written, length(Entries), Entries, cmkit:elapsed(T0)}),
    {ok, Tree2}.

write_entries(Pid, Tree, Entries) ->
    {ok, Tree2} = with_entries(Tree, Entries),
    write_tree(Pid, Tree2).

write_tree(Pid, Tree) ->
    Root = cbt_btree:get_state(Tree),
    Header = {1, Root},
    cbt_file:write_header(Pid, Header),
    {ok, Tree}.

replicate(Action, #{ replicator := R}) ->
    cmdb_replicator:replicate(R, out, Action);

replicate(_, _) -> ok.

pipeline( _, _, [], _, Entries) -> {ok, lists:reverse(Entries)};
pipeline(Tree, Context, [I|Rem], #{ name := Name }=Data, Entries) ->
    case cmencode:encode(I, Context) of 
        {ok, skip} ->
            pipeline(Tree, Context, Rem, Data, Entries);

        {ok, Encoded} ->
            case pipeline_item(Tree, Context, Encoded, Data) of 
                {ok, Context2, Entry} ->
                    pipeline(Tree, Context2, Rem, Data, [Entry|Entries]);
                {ok, Context2} ->
                    pipeline(Tree, Context2, Rem, Data, Entries)
            end;
        Other ->
            cmkit:warning({Name, pipeline, I, skipped, Other}),
            pipeline(Tree, Context, Rem, Data, Entries)
    end.

pipeline_item(Tree, Context, #{ subject := S,
                                predicate := P,
                                object := O,
                                as := As }, _) ->
    
    case cmdb_util:get(disc, Tree, S, P, O) of
        {ok, [{S, P, O, V}]} ->
            {ok, Context#{ As => V }};
        _Other ->
            {ok, Context}
    end;

pipeline_item(_, Context, #{ subject := S,
                             predicate := P,
                             object := O,
                             value := V }, #{ host := H,
                                              timestamp_fun := TFun }) ->

    {ok, Context, {{S, P, O, H, TFun()}, V}}.        


