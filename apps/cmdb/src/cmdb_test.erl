-module(cmdb_test).
-export([test_cases/0,
         test/0,
         stress/4]).


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

    
test() ->
    lists:map(fun({O, I}) ->
                      O = cmdb_util:merge(I)
              end, test_cases()).




% 100M entries
% 4 threads = 25 M entries/thread
% Batches of 100 =
% Sleep 1s
% Time to write 1M = 100s * time to write 10K
%
%
stress(Name, N, C, I) ->
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
