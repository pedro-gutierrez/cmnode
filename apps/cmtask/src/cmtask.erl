-module(cmtask).
-export([schedule/2]).

schedule(Task, Params) ->
    case cmtask_sup:new_task(Task, Params) of 
        {ok, Pid} ->
            cmtask_worker:run(Pid),
            ok;
        Other -> 
            Other
    end.
