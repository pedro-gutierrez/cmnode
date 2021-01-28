-module(cmeffect_cloud).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> cloud.
effect_apply(#{ query := nodes }, SessionId) ->
    CurrentNodes = cmcloud:current_nodes(),
    ExpectedNodes= cmcloud:expected_nodes(),
    Nodes = lists:map(fun(N) ->
                              #{ name => N,
                                 status => node_status(N, CurrentNodes)
                               }
                      end, ExpectedNodes),

    cmcore:update(SessionId, #{ nodes => Nodes }).

node_status(N, Nodes) ->
    case lists:member(N, Nodes) of 
        true -> online;
        false -> offline
    end.
