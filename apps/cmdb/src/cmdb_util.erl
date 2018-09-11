-module(cmdb_util).
-export([
         reload/0,
         open/2,
         reset/2,
         delete/2,
         put/3,
         get/4,
         get/5,
         stress/4
        ]).
-define(ETS_OPTS, [
                   public, 
                   ordered_set, 
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

    cmcode:compile(#{ module => cmdb_config,
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
                 cmkit:reregister(Name, Fd);
        no_valid_header -> 
            {ok, T} = cbt_btree:open(nil, Fd),
            {ok, T2} = cbt_btree:add(T, [{{0, 0, 0}, cmkit:micros()}]),
            Root = cbt_btree:get_state(T2),
            Header = {1, Root},
            cbt_file:write_header(Fd, Header),
            cmkit:reregister(Name, Fd)
    end,
    {ok, Fd}.

resolve(Name, Fun) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_bucket};
        Pid ->
            Fun(Pid)
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
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
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
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, Entries3})
               end).


get(memory, Name, S, P) -> 
    {ok, [V|| [_, V] <- ets:match(Name, {{S, P, '$1'}, '$2'})]}; 

get(disc, Name, S, P) ->
    FoldFun = fun({{S0, P0, 0}, _}, Acc) when S0 =:= S andalso P0 =:= P ->
                      {ok, Acc};
                 ({{S0, P0, _}, V}, Acc) when S0 =:= S andalso P0 =:= P ->
                      {ok, [V|Acc]};
                 (_, Acc) ->
                      {stop, Acc}
              end,
    resolve(Name, fun(Fd) ->
                     {ok, Header, _} = cbt_file:read_header(Fd),
                     {_, Root} = Header,
                     {ok, Tree} = cbt_btree:open(Root, Fd),
                     case cbt_btree:fold(Tree, FoldFun, [], [{start_key, {S, P, 0}}]) of
                         {ok, _, Values} -> {ok, Values};
                         Other ->
                             Other
                     end
             end).

get(memory, Name, S, P, O) -> 
    case ets:lookup(Name, {S, P, O}) of 
        [] -> not_found;
        [{{S, P, O}, V}] -> 
            {ok, V};
        Other -> 
            {error, Other}
    end;

get(disc, Name, S, P, O) ->
    resolve(Name, fun(Fd) -> 
                     {ok, Header, _} = cbt_file:read_header(Fd),
                     {_, Root} = Header,
                     {ok, Tree} = cbt_btree:open(Root, Fd),
                     case cbt_btree:lookup(Tree, [{S, P, O}]) of 
                         [not_found] -> not_found;
                         [{ok, {{S, P, O}, V}}] -> {ok, V};
                         Other -> 
                             {error, Other}
                     end
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
                                                                               cmdb:put(Name, [{ users, is, Id(P, It, K), <<"plop">>} || K <- lists:seq(1, N)])


                                                                       end, lists:seq(1, I))




                                                 end),
                                        cmkit:log(Res),
                                        cmkit:success({stress, P, I, N, T, (N*I)/T*1000000})
                                end)



                  end, lists:seq(1, C)).
