-module(cmcluster_util).
-export([ping/2]).


ping(Nodes, N) ->
    lists:map(fun(N0) when N0 =/= N ->
                      net_adm:ping(N0);
                 (_) -> 
                      self
              end, Nodes).
