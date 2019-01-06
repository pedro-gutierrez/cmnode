-module(cmcluster_util).
-export([ping/2]).


ping(Nodes, N) ->
    lists:foreach(fun(N0) when N0 =/= N ->
                          net_adm:ping(N0);
                     (_) -> 
                          ok
                  end, Nodes).
