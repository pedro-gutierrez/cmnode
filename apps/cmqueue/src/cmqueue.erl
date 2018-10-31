-module(cmqueue).
-export([
         clear/1,
         cancel/2,
         status/1, 
         schedule/2,
         subscribe/3,
         notify/3,
         finish/2
        ]).

clear(Name) ->
    ask(Name, clear).

cancel(Name, Id) ->
    ask(Name, {cancel, Id}).

finish(Name, Id) -> 
    tell(Name, {finished, Id}).

status(Name) -> 
    ask(Name, status).

schedule(Name, Job) ->
    ask(Name, {schedule, Job}).

subscribe(Name, Topic, SessionId) ->
    ask(Name, {subscribe, Topic, SessionId}).

notify(Name, Id, Info) ->
    tell(Name, {info, Id, Info}).

ask(Name, Msg) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_queue};
        Pid -> gen_statem:call(Pid, Msg)
    end.

tell(Name, Msg) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_queue};
        Pid -> gen_statem:cast(Pid, Msg)
    end.
