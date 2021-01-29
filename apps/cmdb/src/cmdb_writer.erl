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

start_link(Bucket) ->
    gen_server:start_link(?MODULE, [Bucket], []).

init([#{ name := Name }=B]) ->
    Data = start(B),
    Topic = cmdb_util:writer(Name),
    cmbus:create_sub(Topic),
    {ok, Data}.

handle_call(reset, _, Data) ->
    {ok, Data2} = reset(Data),
    {reply, ok, Data2};

handle_call(restart, _, Data) ->
    {ok, Data2} = restart(Data),
    {reply, ok, Data2};

handle_call({put, Entries}, _, Data) ->

    write_and_reply(distinct(Entries), Data);

handle_call({delete, Specs}, _, #{ tree := Tree} = Data) ->

    Entries = cmdb_util:read_all(Tree, Specs),
    write_and_reply([], [{S, P, O} || {S, P, O, _} <- Entries], Data);

handle_call({insert, Entries}, _, #{ tree := Tree }=Data) ->

    case cmdb_util:all_new(Tree, Entries) of 
        false ->
            {reply, conflict, Data};
        true ->
            write_and_reply(distinct(Entries), Data)
    end;

handle_call({merge, S, Decoder, Merge}, _, #{ tree := Tree }=Data) ->

    Entries = unwind_merge(Tree, {S, Decoder}, Merge),
    write_and_reply(Entries, Data);

handle_call({merge, S, P, Decoder, Merge}, _, #{ tree := Tree }=Data) ->

    Entries = unwind_merge(Tree, {S, P, Decoder}, Merge),
    write_and_reply(Entries, Data);

handle_call({merge, S, P, O, Decoder, Merge}, _, #{ tree := Tree }=Data) ->

    Entries = unwind_merge(Tree, {S, P, O, Decoder}, Merge),
    write_and_reply(Entries, Data);

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

open(Name) ->
    Filename = filename:join([cmkit:data(), atom_to_list(Name) ++ ".cbt"]),
    {ok, Pid} = cbt_file:open(Filename, [create_if_missing]),
    R = case cbt_file:read_header(Pid) of
            {ok, _Header, _} ->
                {ok, Pid};
            no_valid_header ->
                {ok, T} = cbt_btree:open(nil, Pid),
                case write_entries(Pid, T, [{{0, 0, 0},0}]) of 
                    {ok, _} ->
                        {ok, Pid};
                    Other ->
                        Other
                end
        end,

    case R of 
        {ok, Pid} ->
            Topic = cmdb_util:reader(Name),
            cmbus:create(Topic),
            cmbus:sub(Topic, Pid),
            {ok, Pid};
        Other2 ->
            Other2
    end.

start(#{ name := Name,
         debug := Debug } = B) ->
    Log = cmkit:log_fun(Debug),
    {ok, Pid} = open(Name),
    Ref = erlang:monitor(process, Pid),
    {ok, Header, _} = cbt_file:read_header(Pid),
    {_, Root} = Header,
    {ok, Tree} = cbt_btree:open(Root, Pid),
    Log({cmdb, Name}),
    B#{ host => node(),
        tree => Tree, 
        log => Log,
        pid => Pid, 
        ref => Ref }.


restart(Data) -> 
    restart(Data, []).

restart(#{ log := Log,
           name := Name,
           pid := Fd,
           ref := Ref} = Data, Opts) ->

    erlang:demonitor(Ref),
    ok = cbt_file:close(Fd),
    Filename = filename:join([cmkit:data(), atom_to_list(Name) ++ ".cbt"]),
    case lists:member(delete, Opts) of
        true ->
            ok = file:delete(Filename),
            ok;
        false -> 
            ok
    end,
    Data2 = start(Data),
    Log({Name, resetted}),
    {ok, Data2}.


reset(Data) ->
    restart(Data, [delete]).

distinct(E) ->
    maps:to_list(lists:foldl(fun({S, P, O, V}, Idx) ->
                                     Idx#{ {S, P, O} => V};
                                (_, Idx) -> 
                                     Idx
                             end, #{}, E)).

unwind_merge(Tree, Match, Merge) ->
    case cmdb_util:unwind(Tree, Match) of 
        [] -> 
            [];
        Items ->
            lists:map(fun({S, P, O, V0}) ->
                              {{S, P, O}, cmkit:merge(V0, Merge)}
                      end, Items)
    end.

write_and_reply(ToAdd, Data) ->
    write_and_reply(ToAdd, [], Data).

write_and_reply(ToAdd, ToRemove, #{ pid := Pid,
                                    tree := Tree}=Data) ->
    case write_entries(Pid, Tree, ToAdd, ToRemove) of 
        {ok, Tree2} ->
            {reply, ok, Data#{ tree => Tree2}};
        Other ->
            {reply, Other, Data}
    end.

write_entries(Pid, Tree, ToAdd) ->
    write_entries(Pid, Tree, ToAdd, []).

write_entries(_, Tree, [], []) -> {ok, Tree};

write_entries(Pid, Tree, ToAdd, ToRemove) -> 

    {ok, Tree2} = cbt_btree:add_remove(Tree, ToAdd, ToRemove),
    Root = cbt_btree:get_state(Tree2),
    Header = {1, Root},
    case cbt_file:write_header(Pid, Header) of 
        {ok, _} ->
            {ok, Tree2};
        {error, E} ->
            {error, E}
    end.
