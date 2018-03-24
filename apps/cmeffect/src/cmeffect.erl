-module(cmeffect).
-export([apply/3]).

apply(Effect, Data, Session) ->
    case erlang:whereis(Effect) of 
        undefined ->
            cmkit:log({effect, Effect, undefined}),
            not_found;
        Pid ->
            gen_server:cast(Pid, {apply, Data, Session}),
            ok
    end.



