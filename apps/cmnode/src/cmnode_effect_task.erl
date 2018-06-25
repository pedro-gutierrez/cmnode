-module(cmnode_effect_task).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> task.
effect_apply(#{ task := Task,
                params := Params }, _) ->

    cmtask:schedule(Task, Params).

