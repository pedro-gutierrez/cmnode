-module(cmsession).
-export([new/1, attach/3, delete/1, retrieve/2, conns/1, tell/2, stream/2, broadcast/1]).

new(App) ->
    Conn = self(),
    Id = cmkit:uuid(),
    Session = #{ id => Id, app => App },
    Entries = [{session, with_id, Id, Session}, 
               {Id, has_connection, Conn, Conn},
               {connection, with_pid, Conn, Conn },
               {Conn, has_session, Id, Id}],
    case cmdb:put(sessions, Entries) of 
        ok -> {ok, Session};
        Other -> 
            cmkit:danger({cmsession, new, Other, App, Entries}),
            {error, session_not_created}
    end.

attach(Id, Type, Val) ->
    cmdb:put(sessions, [{Id, has, Type, Val}]).


retrieve(Id, Type) ->
    cmdb:first(sessions, Id, has, Type).

delete(Id) ->
    cmkit:warning({cmsession, delete, Id, pending}),
    ok.

conns(Id) ->
    case cmdb:all(sessions, Id, has_connection) of 
        not_found -> 
            cmkit:warning({cmsession, no_connections, Id}),
            {ok, []};
        {ok, Conns} -> 
            {ok, Conns}
    end.
    
tell(Id, Data) ->
    {ok, Conns} = conns(Id),
    [ C ! Data || C <- Conns ].

stream(Id, {Ev, Data}) ->
    {ok, Conns} = conns(Id),
    [ C ! {stream, Ev, Data} || C <- Conns ].

broadcast(Data) ->
    {ok, Conns } = cmdb:all(sessions, connection, with_pid), 
    [ C ! Data || C <- Conns ].
