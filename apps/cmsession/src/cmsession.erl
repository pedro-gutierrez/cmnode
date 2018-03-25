-module(cmsession).
-export([new/1, attach/3, retrieve/2, tell/2]).

new(App) ->
    Conn = self(),
    Id = cmkit:uuid(),
    Session = #{ id => Id, app => App },
    Entries = [{{session, Id}, Session}, 
               {{connections, Id}, [Conn]},
               {{connection, Conn}, #{ pid => Conn, session => Id }}],
    ok = cmdb:put_new(sessions, Entries),
    {ok, Session}.

attach(Id, Type, Val) ->
    cmdb:put(sessions, {Type, Id}, Val).

retrieve(Id, Type) ->
    cmdb:get(sessions, {Type, Id}).

conns(Id) ->
    {ok, [Conns]} = cmdb:get(sessions, {connections, Id}),
    {ok, Conns}.
    
tell(Id, Data) ->
    {ok, Conns} = conns(Id),
    [ C ! Data || C <- Conns ].
