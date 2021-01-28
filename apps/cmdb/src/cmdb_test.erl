-module(cmdb_test).

-export([f/0, m/0, s/0, s/1, s/4]).

-export([bag/1, delete1/1, distinct/1, empty/1,
         insert1/1, match1/1, merge1/1, merge2/1, merge3/1,
         merge4/1, merge5/1, multiple_get/1, sorted_keys1/1,
         sorted_keys2/1, update/1]).

-export([m1/0, m10/0, m11/0, m12/0, m2/0, m3/0, m4/0,
         m5/0, m6/0, m7/0, m8/0, m9/0]).

all_f() ->
    [empty, insert1, distinct, bag, sorted_keys1,
     sorted_keys2, merge1, merge2, merge3, merge4, merge5,
     multiple_get, match1, delete1].

all_m() ->
    [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12].

f() -> f(test).

f(Name) ->
    lists:foreach(fun (T) ->
                          ok = cmdb:reset(Name), (?MODULE):T(Name)
                  end,
                  all_f()).

empty(Name) ->
    [] = cmdb:get(Name, a, b, c),
    [] = cmdb:get(Name, a, b),
    [] = cmdb:get(Name, a).

insert1(Name) ->
    E = {a, b, c, 1},
    Entries = [E],
    ok = cmdb:insert(Name, Entries),
    conflict = cmdb:insert(Name, Entries).

distinct(Name) ->
    E = {a, b, c, 1},
    Entries = [E, E],
    ok = cmdb:put(Name, Entries),
    [E] = cmdb:get(Name, a, b, c).

update(Name) ->
    E1 = {a, b, c, 1},
    E2 = {a, b, c, 2},
    ok = cmdb:put(Name, [E1]),
    [E1] = cmdb:get(Name, a, b, c),
    ok = cmdb:put(Name, [E2]),
    [E2] = cmdb:get(Name, a, b, c).

bag(Name) ->
    E1 = {a, b, c, 1},
    E2 = {a, b, c, 2},
    ok = cmdb:put(Name, [E2, E1, E2]),
    [E2] = cmdb:get(Name, a, b, c).

sorted_keys1(Name) ->
    E1 = {a, b, 1, 0},
    E2 = {a, b, 2, 0},
    ok = cmdb:put(Name, [E1, E2]),
    [E1, E2] = cmdb:get(Name, a, b),
    ok = cmdb:put(Name, [E2, E1]),
    [E1, E2] = cmdb:get(Name, a, b).

sorted_keys2(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, c, b, 0},
    ok = cmdb:put(Name, [E1, E2]),
    [E1, E2] = cmdb:get(Name, a),
    ok = cmdb:put(Name, [E2, E1]),
    [E1, E2] = cmdb:get(Name, a).

merge1(Name) ->
    E1 = {a, b, c, 0},
    ok = cmdb:put(Name, [E1]),
    ok = cmdb:merge(Name, a, b,
                    #{type => number, value => 0}, 1),
    [{a, b, c, 1}] = cmdb:get(Name, a, b, c).

merge2(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, b, d, 0},
    ok = cmdb:put(Name, [E1, E2]),
    ok = cmdb:merge(Name, a, b,
                    #{type => number, value => 0}, 1),
    [{a, b, c, 1}] = cmdb:get(Name, a, b, c),
    [{a, b, d, 1}] = cmdb:get(Name, a, b, d).

merge3(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, b, d, a},
    ok = cmdb:put(Name, [E1, E2]),
    ok = cmdb:merge(Name, a, b,
                    #{type => number, value => 0}, 1),
    [{a, b, c, 1}] = cmdb:get(Name, a, b, c),
    [{a, b, d, a}] = cmdb:get(Name, a, b, d).

merge4(Name) ->
    E1 = {a, b, c, #{a => b}},
    ok = cmdb:put(Name, [E1]),
    ok = cmdb:merge(Name, a, b,
                    #{type => object,
                      spec => #{a => #{type => keyword, value => b}}},
                    #{c => d}),
    [{a, b, c, #{a := b, c := d}}] = cmdb:get(Name, a, b,
                                              c).

merge5(Name) ->
    E1 = {a, b, c, #{a => b}},
    E2 = {a, b, d, #{a => b}},
    ok = cmdb:put(Name, [E1, E2]),
    ok = cmdb:merge(Name, a, b, c,
                    #{type => object,
                      spec => #{a => #{type => keyword, value => b}}},
                    #{a => c}),
    [{a, b, c, #{a := c}}] = cmdb:get(Name, a, b, c),
    [{a, b, d, #{a := b}}] = cmdb:get(Name, a, b, d).

multiple_get(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, c, b, 0},
    ok = cmdb:put(Name, [E1, E2]),
    [E1, E2] = cmdb:get(Name, [{a, b, c}, {a, c, b}]).

match1(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, d, b, 1},
    ok = cmdb:put(Name, [E1, E2]),
    [{a, b, c, 0}] = cmdb:match(Name, a,
                                #{type => number, value => 0}),
    [{a, d, b, 1}] = cmdb:match(Name, a,
                                #{type => number, value => 1}).

delete1(Name) ->
    E1 = {a, b, c, 0},
    E2 = {a, b, d, 0},
    E3 = {a, d, b, 1},
    ok = cmdb:put(Name, [E1, E2, E3]),
    ok = cmdb:delete(Name, [{a, b}, {a, d, b}]),
    [] = cmdb:get(Name, a).

s() -> s(test).

s(Name) -> s(Name, 500, 100, 10).

%% N = Batch Size
%% C = Concurrency
%% I = Iterations per Batch
s(Name, N, C, I) ->
    ok = cmdb:reset(Name),
    Collector = spawn_link(fun () ->
                                   loop(0, 0, C, cmkit:micros())
                           end),
    lists:foreach(fun (P) ->
                          spawn_link(fun () ->
                                             cmkit:log({writer, starting}),
                                             lists:foreach(fun (It) ->
                                                                   Entries =
                                                                       [{N0, P,
                                                                         It,
                                                                         <<"plop">>}
                                                                        || N0
                                                                               <- lists:seq(1,
                                                                                            N)],
                                                                   {T, _} =
                                                                       timer:tc(fun
                                                                                    () ->
                                                                                       cmdb:put(Name,
                                                                                                Entries)
                                                                               end),
                                                                   Collector !
                                                                       {N, T}
                                                           end,
                                                           lists:seq(1, I)),
                                             Collector ! finished,
                                             cmkit:log({writer, finished})
                                     end)
                  end,
                  lists:seq(1, C)).

loop(SoFar, Writers, Writers, Since) ->
    Elapsed = cmkit:elapsed(Since) / 1000000,
    Speed = SoFar / Elapsed,
    cmkit:success({finished,
                   #{inserted => SoFar, seconds => Elapsed,
                     speed => trunc(Speed)}});
loop(SoFar, Finished, Writers, Since) ->
    receive
        finished -> loop(SoFar, Finished + 1, Writers, Since);
        {Count, T} ->
            cmkit:log({written, SoFar + Count,
                       trunc(1000000 * Count / T)}),
            loop(SoFar + Count, Finished, Writers, Since)
    end.

m() -> lists:foreach(fun (T) -> m(T) end, all_m()).

m(T) ->
    {Out, In} = (?MODULE):T(),
    {{S, P, O, _}, V2, Seen2, F2} = lists:foldl(fun (E,
                                                     {{S0, P0, O0, H0}, V, Seen,
                                                      F}) ->
                                                        cmdb_util:m2(E, S0, P0,
                                                                     O0, H0, V,
                                                                     Seen, F)
                                                end,
                                                {{n, n, n, n}, n, n, []}, In),
    {_, F3} = cmdb_util:maybe_keep(S, P, O, V2, Seen2, F2),
    case F3 of
        Out -> ok;
        Other ->
            cmkit:danger({cmdb_teest, m, T,
                          #{input => In, expected => Out, actual => Other}}),
            ok
    end.

m1() ->
    {[{a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>},
      {a, a, a, a0, 2, <<"v1">>}]}.

m2() ->
    {[{a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>},
      {a, a, a, a1, 1, <<"v1">>}]}.

m3() ->
    {[{a, a, a, <<"v2">>}, {a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>}, {a, a, a, a1, 1, <<"v1">>},
      {a, a, a, a2, 1, <<"v2">>}]}.

m4() ->
    {[{a, a, b, <<"v1">>}, {a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>},
      {a, a, b, a0, 2, <<"v1">>}]}.

m5() ->
    {[{a, a, b, <<"v1">>}, {a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>}, {a, a, b, a0, 2, <<"v1">>},
      {a, a, b, a1, 1, <<"v1">>}]}.

m6() ->
    {[{a, a, b, <<"v1">>}, {a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>}, {a, a, b, a1, 1, <<"v1">>},
      {a, a, b, a1, 2, <<"v1">>}]}.

m7() ->
    {[{a, a, b, <<"v2">>}, {a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>}, {a, a, b, a1, 1, <<"v1">>},
      {a, a, b, a1, 2, <<"v2">>}]}.

m8() ->
    {[{a, a, b, <<"v2">>}, {a, a, a, <<"v3">>}],
     [{a, a, a, a0, 1, <<"v1">>}, {a, a, a, a0, 1, <<"v3">>},
      {a, a, b, a1, 2, <<"v2">>}]}.

m9() ->
    {[{a, a, b, <<"v2">>}, {a, a, a, <<"v3">>},
      {a, a, a, <<"v1">>}],
     [{a, a, a, a0, 1, <<"v1">>}, {a, a, a, a1, 1, <<"v3">>},
      {a, a, b, a1, 2, <<"v2">>}]}.

m10() ->
    {[{a, c, a, <<"v2">>}, {a, b, a, <<"v1">>}],
     [{a, b, a, a0, 1, <<"v1">>},
      {a, c, a, a0, 1, <<"v2">>}]}.

m11() ->
    {[{a, c, a, <<"v3">>}, {a, c, a, <<"v2">>},
      {a, b, a, <<"v1">>}],
     [{a, b, a, a0, 1, <<"v1">>}, {a, c, a, a0, 1, <<"v2">>},
      {a, c, a, a1, 1, <<"v3">>}]}.

m12() ->
    {[{b, a, a, <<"v1">>}, {a, c, a, <<"v2">>},
      {a, b, a, <<"v1">>}],
     [{a, b, a, a0, 1, <<"v1">>}, {a, c, a, a0, 1, <<"v2">>},
      {b, a, a, a0, 1, <<"v1">>}]}.
