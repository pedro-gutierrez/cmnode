-module(cmeffect).
-export([apply/3]).

apply(Effect, Data, SessionId) ->
    case erlang:whereis(Effect) of 
        undefined ->
            cmkit:danger({effect, not_found, Effect}),
            not_found;
        Pid ->
            gen_server:cast(Pid, {apply, Data, SessionId}),
            ok
    end.
