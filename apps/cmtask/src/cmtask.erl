-module(cmtask).
-export([schedule/2]).

schedule(Task, #{ settings := Settings } = Params) ->
    case cmconfig:task(Task) of 
        {ok, Spec} ->
            case cmconfig:settings(Settings) of 
                {ok, SettingsSpec} ->
                    cmtask_util:run(Spec, SettingsSpec, Params);
                Other  -> Other
            end;
        Other -> Other
    end.
