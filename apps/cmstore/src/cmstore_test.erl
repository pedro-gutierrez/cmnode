-module(cmstore_test).
-export([write/0,
         not_found/0,
         rel/0,
         bench/1]).


write() ->
    ok = cmstore:reset(test),
    A = <<"app">>,
    K = <<"test">>,
    Id = <<"abc">>,
    Ev = <<"created">>,
    Data = #{ <<"key">> => <<"value">> },
    E = #{ app => A, 
           kind => K,
           id => Id,
           event => Ev,
           data => Data},
    ok = cmstore:write(test, E),
    {ok, [#{ app := A,
             kind := K,
             id := Id,
             event := Ev,
             rel := null,
             other := null,
             data := Data,
             node := _,
             timestamp := _ }]} = cmstore:read(test, #{ app => <<"app">>, kind => <<"test">> }).

not_found() ->
    ok = cmstore:reset(test),
    {ok, []} = cmstore:read(test, #{ app => <<"app">>, kind => <<"foo">> }).


rel() ->
    ok = cmstore:reset(test),

    A = <<"app">>,
    K = <<"test">>,
    Id = <<"abc">>,
    R = <<"child-of">>,
    O = <<"def">>,

    Ev = <<"created">>,
    Data = #{ <<"key">> => <<"value">> },
    E = #{ app => A, 
           kind => K,
           id => Id,
           rel => R, 
           other => O,
           event => Ev,
           data => Data},
    ok = cmstore:write(test, E),
    {ok, [#{ app := A,
             kind := K,
             id := Id,
             event := Ev,
             rel := <<"child-of">>,
             other := <<"def">>,
             data := Data,
             node := _,
             timestamp := _ }]} = cmstore:read(test, #{ app => <<"app">>, rel => <<"child-of">>, other => <<"def">> }).

bench(Writes) ->
    ok = cmstore:reset(test),
    {Micros, Res} = timer:tc(fun() ->
                                     write_many(Writes, test, #{ app => <<"app">>, 
                                                                 kind => <<"test">>,
                                                                 id => <<"94f047a5-fa00-4bd6-8c59-c1827a6e1c7e">>,
                                                                 event => <<"created">>,
                                                                 data => #{ <<"key1">> => <<"value">>,
                                                                            <<"key2">> => 1 }})
                             end),
    {Writes, Micros, Res, trunc(Writes/Micros*1000000)}.

write_many(0, _, _) -> ok;
write_many(Rem, Store, E) ->
    case cmstore:write(test, E) of 
        ok -> write_many(Rem-1, Store, E);
        Other ->
            {error, Rem, Other}
    end.
