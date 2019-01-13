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
         put_del/4,
         map/5,
         map/6,
         inspect/2,
         inspect/3,
         inspect/4,
         pipeline/3,
         m2/8,
         maybe_keep/6,
         fold/3,
         reduce/4,
         keys_for/2,
         without_keys/2
        ]).

-define(ETS_OPTS, [
                   public, 
                   ordered_set, 
                   named_table, 
                   {write_concurrency, true}, 
                   {read_concurrency, true}
                  ]).


reload() ->
    Buckets = [ compiled(Spec) || {ok, Spec} <- cmkit:yamls(bucket)] ,
    reload(Buckets).


reload([]) -> {ok, []};
reload(Buckets) ->
    
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
    
    ReplicatorClauses = [ #{ vars => [#{ atom => Name }],
                            body => #{ abstract => 
                                       cmkit:to_atom(cmkit:bin_join([ 
                                                                     cmkit:to_bin(Name), 
                                                                     <<"_replicator">>])) 
                                     }
                          } || #{ name := Name} <- Buckets ],

    HostClauses = [ #{ vars => [],
                       body => #{ abstract => cmkit:node_host_short(node()) }}],

    _Res = cmcode:compile(#{ module => cmdb_config,
                      functions => #{ host => #{ arity => 0,
                                                 clauses => HostClauses },
                                      storage => #{ arity => 1,
                                                    clauses => StorageClauses },
                                      writer => #{ arity => 1,
                                                   clauses => WriterClauses },
                                      replicator => #{ arity => 1,
                                                     clauses => ReplicatorClauses }}}),


    _Res2 = [open(Storage, Name) ||
        #{ name := Name, storage := Storage } <- Buckets ],
    
    {ok, Buckets}.




compiled(#{ <<"name">> := Name,
           <<"spec">> := #{ <<"storage">> := Storage } = Spec}) ->

    #{ name => cmkit:to_atom(Name),
       storage => cmkit:to_atom(Storage),
       cluster => cmcluster:state(),
       debug => cmkit:to_atom(maps:get(<<"debug">>, Spec, <<"false">>))}.
    

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
                       gen_server:call(Pid, reset)
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

%put(disc, Name, [{_, _, _, _, _, _}|_]=Entries) ->
%    Entries2 = [{{S, P, O, H, M}, V}|| {S, P, O, H, M, V} <- Entries],
%    Writer = cmdb_config:writer(Name),
%    resolve(Writer, fun(Pid) ->
%                       gen_server:call(Pid, {put, cmkit:distinct(Entries2)})
%               end);
%
%put(disc, Name, Entries) ->
%    Host = cmdb_config:host(),
%    Now = cmkit:micros(),
%    Entries2 = [{{S, P, O, Host, Now}, V}|| {S, P, O, V} <- Entries],
%    Writer = cmdb_config:writer(Name),
%    resolve(Writer, fun(Pid) ->
%                       gen_server:call(Pid, {put, cmkit:distinct(Entries2)})
%               end).
%

put(disc, Name, Entries) ->
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, Entries})
               end).


put_del(disc, Name, ToAdd, ToDelete) ->
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                            gen_server:call(Pid, {put_delete, ToAdd, ToDelete})
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
    
map(memory, _Name, _S, _P, _Match, _Merge) ->
    {error, not_implemented};

map(disc, Name, S, P, Match, Merge) ->
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {put, S, P, Match, Merge})
               end).



pipeline(memory, _Name, _P) ->
    {error, not_implemented};

pipeline(disc, Name, P) ->
    Writer = cmdb_config:writer(Name),
    resolve(Writer, fun(Pid) ->
                       gen_server:call(Pid, {pipeline, P})
               end).

fold(Fd, Start, Fun) when is_pid(Fd) ->
    {ok, Header, _} = cbt_file:read_header(Fd),
    {_, Root} = Header,
    {ok, Tree} = cbt_btree:open(Root, Fd),
    fold(Tree, Start, Fun);


fold(Name, Start, Fun) when is_atom(Name) ->
    resolve(Name, fun(Fd) ->
                          fold(Fd, Start, Fun)
                  end);



fold(Tree, Start, Fun) ->
    case cbt_btree:fold(Tree, fun({K, V}, {{S0, P0, O0, H0}, V0, Seen, Out} = Acc) ->
                                      case Fun(K, V) of 
                                          {ok, V2} ->
                                              {ok, m2(V2, S0, P0, O0, H0, V0, Seen, Out)};
                                          skip ->
                                              {ok, Acc};
                                          _ ->
                                              {stop, Acc}
                                      end
                              end, {{n, n, n, n}, n, n, []}, [{start_key, Start}]) of
        {ok, _, {{n, n, n, n}, _, _, _}} ->
            {ok, []};
        {ok, _, {{S0, P0, O0, _}, V0, Seen, Out}} -> 
            {_, Out2} = cmdb_util:maybe_keep(S0, P0, O0, V0, Seen, Out),
            {ok, lists:reverse(Out2)};
        Other ->
            Other
    end.









m2({S, P, O, H, _, V}, n, n, n, n, n, n, F) ->
    {{S, P, O, H}, V, [], F};

m2({S0, P0, O0, H0, _, V}, S0, P0, O0, H0, _, Seen, F) ->
    {{S0, P0, O0, H0}, V, Seen, F};

m2({S0, P0, O0, H, _, V}, S0, P0, O0, _, Prev, Seen, F) ->
    {C2, F2} = maybe_keep(S0, P0, O0, Prev, Seen, F),
    {{S0, P0, O0, H}, V, C2, F2};

m2({S, P, O, H, T, V}, S0, P0, O0, _, Prev, Seen, F) ->
    {_, F2} = maybe_keep(S0, P0, O0, Prev, Seen, F),
    m2({S, P, O, H, T, V}, n, n, n, n, n, n, F2).


maybe_keep(S, P, O, V, Seen, F) ->
    case lists:member(V, Seen) of
        false ->
            {[V|Seen], [{S, P, O, V}|F]};
        true ->
            {Seen, F}
    end.




first_key({S, P, O}) -> {S, P, O, 0, 0};
first_key({S, P}) -> {S, P, 0, 0, 0};
first_key({S}) -> {S, 0, 0, 0, 0};
first_key(_) -> {0, 0, 0, 0, 0}.


is_same_key_fun({S, P, O}) ->
    fun({S0, P0, O0, _, _}, _) when S0 =:= S andalso P0 =:= P andalso O0 =:= O -> true;
       (_, _) -> false
    end;

is_same_key_fun({S, P}) ->
    fun({S0, P0, _, _, _}, _) when S0 =:= S andalso P0 =:= P -> true;
       (_, _) -> false
    end;

is_same_key_fun({S}) ->
    fun({S0, _, _, _, _}, _) when S0 =:= S -> true;
       (_, _) -> false
    end;

is_same_key_fun(_) ->
    fun(_, _) -> stop end.


keys_for(K, Tree) ->
    filter_map(Tree, first_key(K), is_same_key_fun(K), fun(K0, _) -> K0 end).


without_keys(Keys, Tree) ->
    cbt_btree:add_remove(Tree, [], Keys).

reduce(Tree, _, _, []) ->
    {ok, Tree};

reduce(Tree, Map, Reduce, [K|Rem]) ->
    case Map(K, Tree) of 
        {ok, Mapped} ->
            case Reduce(Mapped, Tree) of 
                {ok, Tree2} ->
                    reduce(Tree2, Map, Reduce, Rem);
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

filter_map(Tree, Start, Filter, Map) ->
    case cbt_btree:fold(Tree, fun({K, V}, Acc) ->
                                      case Filter(K, V) of
                                          true  ->
                                              {ok, [Map(K, V)|Acc]};
                                          false ->
                                              {ok, Acc};
                                          stop ->
                                              {stop, Acc}
                                      end
                              end, [], [{start_key, Start}]) of
        {ok, _, Acc} ->
            {ok, Acc};
        Other ->
            Other
    end.

inspect(Name, S) ->
    get(cmdb_config:storage(Name), Name, S).

inspect(Name, S, P) ->
    get(cmdb_config:storage(Name), Name, S, P).

inspect(Name, S, P, O) ->
    get(cmdb_config:storage(Name), Name, S, P, O).
