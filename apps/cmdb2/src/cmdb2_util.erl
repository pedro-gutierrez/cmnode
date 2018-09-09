-module(cmdb2_util).
-export([
         reload/0,
         open/2,
         reset/2,
         delete/2,
         put/3,
         get/4,
         get/5,
         tc/2,
         stress/4
        ]).
-define(ETS_OPTS, [
                   public, 
                   set, 
                   named_table, 
                   {write_concurrency, true}, 
                   {read_concurrency, true}
                  ]).


reload() ->
    Buckets = [ #{ name => cmkit:to_atom(Name),
                   storage => cmkit:to_atom(Storage) } 
                || {ok, #{ <<"name">> := Name,
                           <<"spec">> := #{ <<"storage">> := Storage }}} 
                   <- cmkit:yamls(bucket) ],

    StorageClauses = [ #{ vars => [#{ atom => Name }],
                          body => #{ abstract => Storage }
                        } || #{ name := Name,
                                storage := Storage } <- Buckets ],
    
    WriterClauses = [ #{ vars => [#{ atom => Name }],
                         body => #{ abstract => 
                                    cmkit:to_atom(cmkit:bin_join([ 
                                                                  cmkit:to_bin(Name), 
                                                                  <<"_writer">>])) 
                                  }
                       } || #{ name := Name} <- Buckets ],

    cmcode:compile(#{ module => cmdb2_config,
                      functions => #{ storage => #{ arity => 1,
                                                    clauses => StorageClauses },
                                      writer => #{ arity => 1,
                                                   clauses => WriterClauses }}}),

    [open(Storage, Name) ||
        #{ name := Name, storage := Storage } <- Buckets ],
    
    {ok, Buckets}.

open(memory, Name) ->
    try 
        case ets:new(Name, ?ETS_OPTS) of
            {error, E} -> {error, E};
            _ -> {ok, Name}
        end
    catch
        _:_ -> 
            {error, Name}
    end;
        
open(disc, Name) ->
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

delete(disc, Name) ->
    Filename = filename:join([cmkit:data(), atom_to_list(Name) ++ ".cbt"]),
    ok = file:delete(Filename),
    ok.

reset(memory, Name) -> 
    case ets:delete_all_objects(Name) of 
        true -> ok;
        Other -> 
            Other
    end;

reset(disc, Name) -> 
    Writer = cmdb2_config:writer(Name),
    tc(Writer, fun(Pid) ->
                       gen_server:call(Pid, close)
               end).

put(memory, Name, Entries) -> 
    Entries2 = [{{S, P, O}, V}|| {S, P, O, V} <- Entries],
    case ets:insert(Name, Entries2) of
        true ->
            ok;
        Other ->
            Other
    end;

put(disc, Name, [{S0, P0, _, _}|_]=Entries) ->
    Entries2 = [{{S, P, O}, V}|| {S, P, O, V} <- Entries],
    Entries3 = [{{S0, P0, 0}, cmkit:micros()}|Entries2],
    Writer = cmdb2_config:writer(Name),
    tc(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, Entries3})
               end).


get(memory, Name, S, P) -> 
    ets:match(Name, {{S, P, '_'}, '$1'});

get(disc, Name, S, P) ->
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

get(memory, Name, S, P, O) -> 
    ets:lookup(Name, {S, P, O});

get(disc, Name, S, P, O) ->
    tc(Name, fun(Fd) -> 
                     {ok, Header, _} = cbt_file:read_header(Fd),
                     {_, Root} = Header,
                     {ok, Tree} = cbt_btree:open(Root, Fd),
                     cbt_btree:lookup(Tree, [{S, P, O}])
             end).

stress(Name, N, C, I) ->
    Id = fun(P, It, K) ->
                 PBin = cmkit:to_bin(P),
                 KBin = cmkit:to_bin(K),
                 ItBin = cmkit:to_bin(It),
                 <<PBin/binary, "-", ItBin/binary, "-", KBin/binary>>
         end,

    lists:foreach(fun(P) -> 
                          spawn(fun() ->
                                        { T, Res } = timer:tc(fun() -> 
                                                         lists:foreach(fun(It) ->
                                                                               cmdb2:put(Name, [{ users, is, Id(P, It, K), <<"plop">>} || K <- lists:seq(1, N)])


                                                                       end, lists:seq(1, I))




                                                 end),
                                        cmkit:log(Res),
                                        cmkit:success({stress, P, I, N, T, (N*I)/T*1000000})
                                end)



                  end, lists:seq(1, C)).
