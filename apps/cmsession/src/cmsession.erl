-module(cmsession).
-export([new/1, attach/3, delete/1, retrieve/2, conns/1, tell/2, stream/2, broadcast/1]).

new(App) ->
    Conn = self(),
    Id = cmkit:uuid(),
    Session = #{ id => Id, app => App },
    Entries = [{sessions, has, Id, Session},
               {Id, has_connection, Conn, Conn},
               {connections, has, Conn, Conn }],
    case cmdb:put(sessions, Entries) of 
        ok -> {ok, Session};
        Other -> 
            cmkit:danger({cmsession, new, Other, App, Entries}),
            {error, session_not_created}
    end.

attach(Id, Type, Val) ->
    cmdb:put(sessions, [{Id, has, Type, Val}]).

retrieve(Id, Type) ->
    case cmdb:get(sessions, Id, has, Type) of 
        {ok, []} -> 
            not_found;
        {ok, [{_, _, _, V}]} ->
            {ok, V};
        {ok, Other} ->
            {error, Other}
    end.

delete(Id) ->
    case conns(Id) of 
        {ok, Pids} ->
            Keys = lists:flatten(
                        [[{Id, has_connection, Pid},
                          {connections, has, Pid}]|| Pid <- Pids]),
            Keys2 = [{sessions, has, Id}|Keys],
            cmdb:del(sessions, Keys2);
        Other ->
            Other
    end.

conns(Id) ->
    case cmdb:get(sessions, Id, has_connection) of 
        {ok, Conns} ->
            {ok, [Pid || {_,_,_,Pid} <- Conns]};
        Other ->
            {error, Other}
    end.
    
tell(Id, Data) ->
    {ok, Conns} = conns(Id),
    [ C ! Data || C <- Conns ].

stream(Id, {Ev, Data}) ->
    {ok, Conns} = conns(Id),
    [ C ! {stream, Ev, Data} || C <- Conns ].

broadcast(Data) ->
    case cmdb:get(sessions, connections, has) of 
        {ok, Conns} ->
            [ Pid ! Data || {_, _, _, Pid} <- Conns ];
        Other ->
            {error, Other}
    end.
