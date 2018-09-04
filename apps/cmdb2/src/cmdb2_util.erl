-module(cmdb2_util).
-export([
         repeat/4,
         open2/0,
         open/0, 
         close/1,
         put/3
        ]).

repeat(Env, Db, Count, It) -> 
    Res = [test(Env, Db, Count, rand:uniform(It)) || _ <- lists:seq(1, It) ],
    TotalTime = lists:foldl(fun({T, _}, Acc) ->
                            Acc + T
                        end, 0, Res),

    TotalEntries = Count * It,
    #{ usec => TotalTime, 
       ops => TotalEntries,
       ops_sec  => trunc(TotalEntries/TotalTime*1000000) }.


test(Env, Db, Count, Prefix) ->

    Entries = [ test_entry(K, Prefix) || K <- lists:seq(1, Count) ],
    put_timer(Env, Db, Entries).


test_entry(K, Prefix) -> 
    { { Prefix, K}, #{ key => K,
                       number => 1,
                       atom => a,
                       list => [a, b, 1],
                       map => #{ test => <<"a">> },
                       pid => self(),
                       text => <<"This is an Erlang NIF for OpenLDAP's Lightning Memory-Mapped Database (LMDB) database libra  ry. LMDB is an fast, compact key-value data store developed by Symas for the OpenLDAP Project. It uses memory-mapped files, so   it has the read performance of a pure in-memory database while still offering the persistence of standard disk-based databases,   and is only limited to the size of the virtual address space, (it is not limited to the size of physical RAM). LMDB was origin  ally called MDB, but was renamed to avoid confusion with other software associated with the name MDB. See the LMDB docs for mor  e information about LMDB itself."/utf8>> }}.


open2() -> 
    cowdb:open({local, cow}, filename:join([cmkit:data(), "cow.db"]), []).

open() -> 
    {ok, Env} = elmdb:env_open(filename:join([cmkit:data(), "mdb"]), [{map_size, 85899345920}]),
    {ok, Db} = elmdb:db_open(Env, [create]),
    {ok, Env, Db}.

close(Env) -> 
    ok = elmdb:env_close(Env).

put_timer(_Env, Db, Entries) ->
    timer:tc(fun() -> 
                     put2(Db, Entries)
             end).

put2(Db, Entries) ->
    cowdb:transact(Db, [{add, K, V}|| {K, V} <- Entries]). 

put(Env, Db, Entries) ->
    {ok, Txn} = elmdb:txn_begin(Env),
    [elmdb:txn_put(Txn, Db, K, V) || {K, V} <- Entries],
    elmdb:txn_commit(Txn).
