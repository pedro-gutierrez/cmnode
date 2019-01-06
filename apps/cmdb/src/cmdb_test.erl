-module(cmdb_test).
-export([f/0, s/0, s/1, s/4]).
-export([distinct/1, 
         bag/1, 
         update/1,
         sorted_keys1/1,
         sorted_keys2/1]).

all() -> [distinct, bag, sorted_keys1, sorted_keys2].


f() ->
    f(test).

f(Name) ->
    lists:foreach(fun(T) ->
                          ok = cmdb:reset(Name),
                          ?MODULE:T(Name)
                  end, all()).

distinct(Name) ->
    E = {a, b, c, 1},
    Entries = [E, E],
    ok = cmdb:put(Name, Entries),
    {ok, [E]} = cmdb:get(Name, a, b, c).

update(Name) ->
    E1 = {a, b, c, 1},
    E2 = {a, b, c, 2},
    ok = cmdb:put(Name, [E1]),
    {ok, [E1]} = cmdb:get(Name, a, b, c),
    ok = cmdb:put(Name, [E2]),
    {ok, [E2]} = cmdb:get(Name, a, b, c).

bag(Name) ->
    E1 = {a, b, c, 1},
    E2 = {a, b, c, 2},
    ok = cmdb:put(Name, [E2, E1, E2]),
    {ok, [E2, E1]} = cmdb:get(Name, a, b, c).


sorted_keys1(Name) ->
    E1 = {a, b, 1, 0},
    E2 = {a, b, 2, 0},
    ok = cmdb:put(Name, [E1, E2]),
    {ok, [E1, E2]} = cmdb:get(Name, a, b),
    ok = cmdb:put(Name, [E2, E1]),
    {ok, [E1, E2]} = cmdb:get(Name, a, b).


sorted_keys2(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, c, b, 0},
    ok = cmdb:put(Name, [E1, E2]),
    {ok, [E1, E2]} = cmdb:get(Name, a),
    ok = cmdb:put(Name, [E2, E1]),
    {ok, [E1, E2]} = cmdb:get(Name, a).

s() -> s(test).

s(Name) ->
    s(Name, 500, 100, 10).

%% N = Batch Size
%% C = Concurrency
%% I = Iterations per Batch
s(Name, N, C, I) ->
    ok = cmdb:reset(Name),
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
    Elapsed = cmkit:elapsed(Since)/1000000,
    Speed = SoFar/Elapsed,
    cmkit:success({finished, SoFar, Elapsed, Speed});

loop(SoFar, Finished, Writers, Since) ->
    receive
        finished ->
            loop(SoFar, Finished+1, Writers, Since);

        {Count, T} ->
            cmkit:log({written, SoFar+Count, trunc(1000000*Count/T)}),
            loop(SoFar+Count, Finished, Writers, Since)
    end.
