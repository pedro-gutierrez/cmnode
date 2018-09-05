-module(cmdb2).
-export([
        open/0,
        add/5,
        add_many/2,
        put/5,
        put/2,
        get/3,
        get/4
        ]).

open() -> 
    [cmdb2_util:open(B) || #{ name := B,
                              storage := disc } <- cmconfig:buckets() ].

put(Bucket, S, P, O, Meta) -> 
    ?MODULE:put(Bucket, [
                 {S, P, O, Meta}
                ]).

add(Bucket, S, P, O, Meta) -> 
    ?MODULE:put(Bucket, [
                 {S, P, 0, cmkit:micros()}, 
                 {S, P, O, Meta}
                ]).

add_many(Bucket, [{S, P, _, _}|_]=Entries) -> 
    ?MODULE:put(Bucket, [{S, P, 0, cmkit:micros()}|Entries]).

put(Bucket, Entries) ->
    cmdb2_util:tc(Bucket, fun(Pid) ->
                                  cmdb2_util:put(Pid, Entries)
                          end).

get(Bucket, S, P) -> 
    cmdb2_util:tc(Bucket, fun(Pid) ->
                                  cmdb2_util:get(Pid, S, P)
                          end).

get(Bucket, S, P, O) ->
    cmdb2_util:tc(Bucket, fun(Pid) ->
                                  cmdb2_util:get(Pid, S, P, O)
                          end).
