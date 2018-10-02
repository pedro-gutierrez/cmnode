-module(cmdb_util).
-export([
         reload/0,
         open/2,
         reset/2,
         delete/2,
         del/3,
         del/4,
         put/3,
         get/3,
         get/4,
         get/5,
         map/5,
         merge/1,
         stress/4,
         test/0
        ]).
-record(acc, {h, v, s, p, o, r}).

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

    HostClauses = [ #{ vars => [],
                       body => #{ abstract => cmkit:node_host_short(node()) }}],

    Res = cmcode:compile(#{ module => cmdb_config,
                      functions => #{ host => #{ arity => 0,
                                                 clauses => HostClauses },
                                      storage => #{ arity => 1,
                                                    clauses => StorageClauses },
                                      writer => #{ arity => 1,
                                                   clauses => WriterClauses }}}),

    cmkit:log({cmdb, config, Res}),

    _Res2 = [open(Storage, Name) ||
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

del(memory, Name, S, P) ->
    case get(memory, Name, S, P) of 
        {ok, Entries} ->
            [ ets:delete(Name, {S0, P0, O0, H, T}) || {S0, P0, O0, H, T, _} <- Entries]; 
        _ ->
            ok
    end;


del(disc, Name, S, P) -> 
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {delete, S, P})
               end).


del(memory, Name, S, P, O) ->
    case get(memory, Name, S, P, O) of 
        {ok, Entries} ->
            [ ets:delete(Name, {S0, P0, O0, H, T}) || {S0, P0, O0, H, T, _} <- Entries]; 
        _ ->
            ok
    end;


del(disc, Name, S, P, O) -> 
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {delete, S, P, O})
               end).


del(memory, Name, Entries) ->
    [del(memory, Name, S, P, O)|| {S, P, O} <- Entries],
    ok;

del(disc, Name, Entries) ->
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {delete, Entries})
               end).

put(memory, Name, Entries) -> 
    Host = cmdb_config:host(),
    Now = cmkit:micros(),
    Entries2 = [{{S, P, O, Host, Now}, V}|| {S, P, O, V} <- Entries],
    case ets:insert(Name, cmkit:distinct(Entries2)) of
        true ->
            ok;
        Other ->
            Other
    end;

put(disc, Name, [{_, _, _, _, _, _}|_]=Entries) ->
    Entries2 = [{{S, P, O, H, M}, V}|| {S, P, O, H, M, V} <- Entries],
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, cmkit:distinct(Entries2)})
               end);

put(disc, Name, Entries) ->
    Host = cmdb_config:host(),
    Now = cmkit:micros(),
    Entries2 = [{{S, P, O, Host, Now}, V}|| {S, P, O, V} <- Entries],
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, cmkit:distinct(Entries2)})
               end).

get(memory, Name, S) -> 
    {ok, [ {S, P, O, H, T, V} 
           || [P, O, H, T, V] <- ets:match(Name, {{S, '$1', '$2', '$3', '$4'}, '$5'})]}; 

get(disc, Name, S) ->
    fold(Name, {S, 0, 0, 0, 0}, fun({S0, P, O, H, M}, V) 
                                      when S0 =:= S ->
                                        {ok, {S0, P, O, H, M, V}};
                                   (_, _) ->
                                        stop
                                end).

get(memory, Name, S, P) -> 
    {ok, [ {S, P, O, H, T, V} 
           || [O, H, T, V] <- ets:match(Name, {{S, P, '$1', '$2', '$3'}, '$4'})]}; 

get(disc, Name, S, P) ->
    fold(Name, {S, P, 0, 0, 0}, fun({S0, P0, O, H, M}, V) 
                                      when S0 =:= S andalso P0 =:= P ->
                                        {ok, {S0, P0, O, H, M, V}};
                                   (_, _) ->
                                        stop
                                end).

get(memory, Name, S, P, O) -> 
    {ok, [ {S, P, O, H, T, V} 
           || [H, T, V] <- ets:match(Name, {{S, P, O, '$1', '$2'}, '$3'})]}; 
get(disc, Name, S, P, O) ->
    fold(Name, {S, P, O, 0, 0}, fun({S0, P0, O0, H, M}, V) 
                                      when S0 =:= S andalso P0 =:= P andalso O0 =:= O->
                                        {ok, {S0, P0, O, H, M, V}};
                                   (_, _) ->
                                        stop
                                end).

map(memory, _Name, _S, _Match, _Merge) ->
    {error, not_implemented};

map(disc, Name, S, Match, Merge) ->
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, S, Match, Merge})
               end).
    
fold(Name, Start, Fun) ->
    resolve(Name, fun(Fd) ->
                          {ok, Header, _} = cbt_file:read_header(Fd),
                          {_, Root} = Header,
                          {ok, Tree} = cbt_btree:open(Root, Fd),
                          case cbt_btree:fold(Tree, fun({K, V}, Acc) ->
                                                            case Fun(K, V) of 
                                                                {ok, V2} ->
                                                                    {ok, [V2|Acc]};
                                                                skip ->
                                                                    {ok, Acc};
                                                                stop ->
                                                                    {stop, Acc}
                                                            end
                                                    end, [], [{start_key, Start}]) of
                              {ok, _, Values} -> {ok, Values};
                              Other ->
                                  Other
                          end
                  end).
   





test_cases() ->[
                {[{a, a, a, <<"v1">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, a, a0, 2, <<"v1">>}
                  ]},
                {[{a, a, a, <<"v1">>}],
                 [ {a, a, a, a0, 1, <<"v1">>},
                   {a, a, a, a1, 1, <<"v1">>} 
                 ]},
                {[{a, a, a, <<"v1">>},
                    {a, a, a, <<"v2">>}],
                 [ {a, a, a, a0, 1, <<"v1">>},
                   {a, a, a, a1, 1, <<"v1">>},
                   {a, a, a, a2, 1, <<"v2">>}
                 ]},
                {[], []},
                {[{a, a, a, <<"v1">>}, 
                  {a, a, b, <<"v1">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, b, a0, 2, <<"v1">>}
                  ]},
                {[{a, a, a, <<"v1">>}, 
                  {a, a, b, <<"v1">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, b, a0, 2, <<"v1">>},
                    {a, a, b, a1, 1, <<"v1">>}
                  ]},
                {[{a, a, a, <<"v1">>}, 
                  {a, a, b, <<"v1">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, b, a1, 1, <<"v1">>},
                    {a, a, b, a1, 2, <<"v1">>}
                  ]},
                {[{a, a, a, <<"v1">>}, 
                  {a, a, b, <<"v2">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, b, a1, 1, <<"v1">>},
                    {a, a, b, a1, 2, <<"v2">>}
                  ]},
                {[{a, a, a, <<"v3">>},
                  {a, a, b, <<"v2">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, a, a0, 1, <<"v3">>},
                    {a, a, b, a1, 2, <<"v2">>}
                  ]},
                {[{a, a, a, <<"v1">>},
                  {a, a, a, <<"v3">>},
                  {a, a, b, <<"v2">>}],
                  [ {a, a, a, a0, 1, <<"v1">>},
                    {a, a, a, a1, 1, <<"v3">>},
                    {a, a, b, a1, 2, <<"v2">>}
                  ]},
                {[{a, b, a, <<"v1">>},
                  {a, c, a, <<"v2">>}],
                  [ {a, b, a, a0, 1, <<"v1">>},
                    {a, c, a, a0, 1, <<"v2">>}]},
                {[{a, b, a, <<"v1">>},
                  {a, c, a, <<"v2">>},
                  {a, c, a, <<"v3">>}],
                  [ {a, b, a, a0, 1, <<"v1">>},
                    {a, c, a, a0, 1, <<"v2">>},
                    {a, c, a, a1, 1, <<"v3">>}]},
                {[{a, b, a, <<"v1">>},
                  {a, c, a, <<"v2">>},
                  {b, a, a, <<"v1">>}],
                  [ {a, b, a, a0, 1, <<"v1">>},
                    {a, c, a, a0, 1, <<"v2">>},
                    {b, a, a, a0, 1, <<"v1">>}]}
               ].

merge(Entries) ->
    #acc{ r = R } = lists:foldl(fun({S, P, O, H, _, V}, #acc{ h = undef, 
                                              v = undef, 
                                              s = undef, 
                                              p = undef, 
                                              o = undef, 
                                              r = R} = Acc) ->
                        Acc#acc{ h = H, 
                                 v = [V], 
                                 s = S, 
                                 p = P, 
                                 o = O,
                                 r = [{S, P, O, V}|R]};

                   ({S, P, O, H, _, _}, #acc{ s = S,
                                              p = P,
                                              o = O,
                                              h = H}=Acc) -> 
                        Acc;
                   ({S, P, O, H, _, V}, #acc{ s = S,
                                              p = P,
                                              o = O,
                                              h = H0,
                                              v = Values,
                                              r = R}=Acc) when H =/= H0 ->
                        case lists:member(V, Values) of 
                            true ->
                                Acc#acc{ h = H };
                            false ->
                                Acc#acc{ h = H,
                                         v = [V|Values],
                                         r = [{ S, P, O, V}|R]}
                        end;
                   ({S, P, O, H, _, V}, #acc{ s = S,
                                              p = P,
                                              o = O0,
                                              r = R}=Acc) when O =/= O0 ->
                        Acc#acc{ h = H,
                                 o = O,
                                 v = [V],
                                 r = [{ S, P, O, V}|R] };

                   ({S, P, O, H, _, V}, #acc{ s = S,
                                              p = P0,
                                              r = R}=Acc) when P =/= P0 ->
                        Acc#acc{ h = H,
                                 o = O,
                                 p = P,
                                 v = [V],
                                 r = [{ S, P, O, V}|R] };

                   ({S, P, O, H, _, V}, #acc{ s = S0,
                                              r = R}=Acc) when S =/= S0 ->
                        Acc#acc{ h = H,
                                 s = S,
                                 o = O,
                                 p = P,
                                 v = [V],
                                 r = [{S, P, O, V}|R] }

                end, #acc{h = undef,
                          v = [], 
                          s = undef,
                          p = undef,
                          o = undef,
                          r = []}, Entries),
    R.
    
test() ->
    lists:map(fun({O, I}) ->
                      O = merge(I)
              end, test_cases()).




% 100M entries
% 4 threads = 25 M entries/thread
% Batches of 100 =
% Sleep 1s
% Time to write 1M = 100s * time to write 10K
%
%
stress(Name, N, C, I) ->
    Id = fun(P, K) ->
                 PBin = cmkit:to_bin(P),
                 KBin = cmkit:to_bin(K),
                 <<PBin/binary, "-", KBin/binary>>
         end,
    Collector = spawn(fun() ->
                    loop(0, 0, C, cmkit:micros())
                 end),
    lists:foreach(fun(P) -> 
                          spawn(fun() ->
                                        cmkit:log({writer, starting}),
                                        lists:foreach(fun(It) ->
                                                              {T, _} = timer:tc(fun() ->
                                                                               cmdb:put(Name, 
                                                                       [{ N0, P, It, <<"plop">>} || N0 <- lists:seq(1, N)])
                                                                       end),
                                                              Collector ! {N, T},
                                                              erlang:garbage_collect()
                                                      end, lists:seq(1, I)),

                                        Collector ! finished,
                                        cmkit:log({writer, finished})
                                end)

                  end, lists:seq(1, C)).

loop(SoFar, Writers, Writers, Since) ->
    cmkit:success({finished, SoFar, cmkit:elapsed(Since)/1000000});

loop(SoFar, Finished, Writers, Since) ->
    receive
        finished ->
            loop(SoFar, Finished+1, Writers, Since);

        {Count, T} ->
            cmkit:log({written, SoFar+Count, trunc(1000000*Count/T)}),
            loop(SoFar+Count, Finished, Writers, Since)
    end.
