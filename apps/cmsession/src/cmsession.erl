-module(cmsession).
-export([new/1, attach/3, retrieve/2, conns/1, tell/2]).

new(App) ->
    Conn = self(),
    Id = cmkit:uuid(),
    Session = #{ id => Id, app => App },
    Entries = [{{session, Id}, Session}, 
               {{connections, Id}, Conn},
               {{connection, Conn}, #{ pid => Conn, session => Id }}],
    case cmdb:put_new(sessions, Entries) of 
        ok -> {ok, Session};
        Other -> 
            cmkit:danger({cmsession, new, Other, App, Entries}),
            {error, session_not_created}
    end.

attach(Id, Type, Val) ->
    cmdb:put(sessions, {Type, Id}, Val).

retrieve(Id, Type) ->
    cmdb:get(sessions, {Type, Id}).

conns(Id) ->
    case cmdb:get(sessions, {connections, Id}) of 
        not_found -> 
            cmkit:warning({cmsession, no_connections, Id}),
            {ok, []};
        {ok, Conns} -> 
            {ok, Conns}
    end.
    
tell(Id, Data) ->
    {ok, Conns} = conns(Id),
    [ C ! Data || C <- Conns ].
