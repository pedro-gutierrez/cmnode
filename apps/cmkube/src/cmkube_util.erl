-module(cmkube_util).
-export([to_query_params/1]).

to_query_params(Map) -> 
    cmkit:map_join(Map, <<"=">>, <<",">>).
