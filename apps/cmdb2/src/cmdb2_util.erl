-module(cmdb2_util).
-export([
         open/2,
         put/3,
         get/4,
         get/5,
         tc/2
        ]).
-define(ETS_OPTS, [
                   public, 
                   set, 
                   named_table, 
                   {write_concurrency, true}, 
                   {read_concurrency, true}
                  ]).


open(ets, Name) ->
    try 
        case ets:new(Name, ?ETS_OPTS) of
            {error, E} -> {error, E};
            _ -> {ok, Name}
        end
    catch
        _:_ -> 
            {error, Name}
    end;
        
open(dets, Name) ->
    Filename = filename:join([cmkit:data(), atom_to_list(Name) ++ ".cbt"]),
    {ok, Fd} = cbt_file:open(Filename, [create_if_missing]),
    ok = case cbt_file:read_header(Fd) of
        {ok, _Header, _} -> 
            case erlang:whereis(Name) of 
                undefined -> 
                    true = register(Name, Fd),
                    ok;
                _ -> 
                    unregister(Name),
                    register(Name, Fd),
                    ok
            end;
        no_valid_header -> 
            {ok, T} = cbt_btree:open(nil, Fd),
            {ok, T2} = cbt_btree:add(T, [{{0, 0, 0}, cmkit:micros()}]),
            Root = cbt_btree:get_state(T2),
            Header = {1, Root},
            cbt_file:write_header(Fd, Header),
            ok
    end,
    {ok, Fd}.

tc(Name, Fun) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_bucket};
        Pid ->
            timer:tc(Fun, [Pid])
    end.

put(ets, Name, Entries) -> 
    Entries2 = [{{S, P, O}, V}|| {S, P, O, V} <- Entries],
    case ets:insert(Name, Entries2) of
        true ->
            ok;
        Other ->
            Other
    end;

put(dets, Name, [{S0, P0, _, _}|_]=Entries) ->
    Entries2 = [{{S, P, O}, V}|| {S, P, O, V} <- Entries],
    Entries3 = [{{S0, P0, 0}, cmkit:micros()}|Entries2],
    Writer = cmdb_config:writer(Name),
    tc(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, Entries3})
               end).


get(ets, Name, S, P) -> 
    ets:match(Name, {{S, P, '_'}, '$1'});

get(dets, Name, S, P) ->
    FoldFun = fun({{S0, P0, 0}, _}, Acc) when S0 =:= S andalso P0 =:= P ->
                      {ok, Acc};
                 ({{S0, P0, O}, V}, Acc) when S0 =:= S andalso P0 =:= P ->
                      {ok, [{O, V}|Acc]};
                 (_, Acc) ->
                      {stop, Acc}
              end,
    tc(Name, fun(Fd) ->
                     {ok, Header, _} = cbt_file:read_header(Fd),
                     {_, Root} = Header,
                     {ok, Tree} = cbt_btree:open(Root, Fd),
                     cbt_btree:fold(Tree, FoldFun, [], [{start_key, {S, P, 0}}])
             end).

get(ets, Name, S, P, O) -> 
    ets:lookup(Name, {S, P, O});

get(dets, Name, S, P, O) ->
    tc(Name, fun(Fd) -> 
                     {ok, Header, _} = cbt_file:read_header(Fd),
                     {_, Root} = Header,
                     {ok, Tree} = cbt_btree:open(Root, Fd),
                     cbt_btree:lookup(Tree, [{S, P, O}])
             end).
