-module(cmqueue).
-export([
         clear/1, 
         status/1, 
         schedule/2,
         subscribe/3
        ]).

clear(Name) ->
    ask(Name, clear).

status(Name) -> 
    ask(Name, status).

schedule(Name, Job) ->
    ask(Name, {schedule, Job}).

subscribe(Name, Topic, SessionId) ->
    ask(Name, {subscribe, Topic, SessionId}).

ask(Name, Msg) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_queue};
        Pid -> gen_statem:call(Pid, Msg)
    end.
