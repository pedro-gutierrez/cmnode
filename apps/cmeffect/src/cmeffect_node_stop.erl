-module(cmeffect_node_stop).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> stop_node.
effect_apply(_, _Id) ->
    %% careful with this
    cmkit:warning({cmnode, node(), stopping}),
    init:stop().
