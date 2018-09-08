-module(cmdb2).
-export([
        open/0,
        put/2,
        all/3,
        first/4,
        stress/4
        ]).

open() -> 
    [cmdb2_util:open(cmdb_config:backend(Name), Name) 
        || #{ name := Name, storage := disc } <- cmconfig:buckets()].

put(Name, Entries) -> 
    cmdb2_util:put(cmdb_config:backend(Name), Name, Entries).

all(Name, S, P) -> 
    cmdb2_util:get(cmdb_config:backend(Name), Name, S, P).

first(Name, S, P, O) ->
    cmdb2_util:get(cmdb_config:backend(Name), Name, S, P, O).


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
                                        cmkit:success({stress, P, I, T/I})
                                end)



                  end, lists:seq(1, C)).

