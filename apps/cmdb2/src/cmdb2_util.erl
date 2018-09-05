-module(cmdb2_util).
-export([
         open/1,
         put/2,
         get/3,
         get/4,
         tc/2
        ]).

open(Name) -> 
    cowdb:open({local, Name}, filename:join([cmkit:data(), atom_to_list(Name) ++ ".db"]), []).


tc(Name, Fun) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_bucket};
        Pid ->
            timer:tc(Fun, [Pid])
    end.

put(Pid, Entries) ->
    cowdb:transact(Pid, [{add, {S, P, O}, V}|| {S, P, O, V} <- Entries]). 

get(Pid, S, P) ->
    cowdb:fold(Pid, fun({{S0, P0, O}, V}, Acc) when S0 =:= S andalso P0 =:= P ->
                        {ok, [{O, V}|Acc]};
                      (_, Acc) ->
                        {stop, Acc}
                   end, [], [{start_key, {S, P, 0}}]).

get(Pid, S, P, O) ->
    cowdb:get(Pid, {S, P, O}).
