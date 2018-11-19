-module(cmcore).
-export([
         init/2, 
         terminate/1, 
         update/2,
         notify/2,
         stream/2
        ]).

init(Spec, #{ app := _, id := _}=Session) ->
    {ok, Pid} = cmcore_context_sup:start_context(Spec, Session, self()),
    erlang:monitor(process, Pid),
    ok = gen_statem:call(Pid, init). 

update(Id, Data) when is_binary(Id) ->
    case global:whereis_name(Id) of 
        undefined ->
            cmkit:warning({cmcore, update, Id, no_such_context});
        Pid ->
            gen_statem:cast(Pid, {update, Data})
    end.

terminate(Id) when is_binary(Id) -> 
    case global:whereis_name(Id) of 
        undefined ->
            cmkit:warning({cmcore, terminate, Id, no_such_context});
        Pid ->
            gen_statem:cast(Pid, terminate)
    end.

notify(Id, Data) ->
    case global:whereis_name(Id) of 
        undefined ->
            cmkit:warning({cmcore, notify, Id, no_such_context});
        Pid ->
            Conns = gen_statem:call(Pid, connections),
            [ C ! Data || C <- Conns ]
    end.

stream(Id, {Ev, Data}) ->
    case global:whereis_name(Id) of 
        undefined ->
            cmkit:warning({cmcore, stream, Id, no_such_context});
        Pid ->
            Conns = gen_statem:call(Pid, connections),
            [ C ! {stream, Ev, Data} || C <- Conns ]
    end.
