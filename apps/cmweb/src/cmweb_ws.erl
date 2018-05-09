-module(cmweb_ws).
-export([
         init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(Req, State) ->
    {cowboy_websocket, Req, State }.

websocket_init(#{app := App, port := Port }=State) ->
    case cmconfig:app(App) of
        {ok, #{ debug := Debug }=Spec} -> 
            Log = cmkit:log_fun(Debug),
            {ok, #{ id := Id }=Session } = cmsession:new(App),
            ok = cmcore:init(#{}, Spec, Session),
            Log({ws, new_connection, App, Port, Id}),
            {ok, State#{ id => Id, log => Log} };
        {error, E} -> 
            cmkit:log({ws, new_connection, invalid_app, App, Port, E}),
            {stop, E}
    end.

websocket_handle({text, Text}, #{ app := App,
                                  port := Port,
                                  id := Id,
                                  log := Log }=State) ->
    case cmkit:jsond(Text) of
        {error, _} -> 
            Log({ws, in, App, Port, Id, invalid, Text}),
            {stop, State};
        {ok, Data} ->
            Log({ws, in, App, Port, Id, self(), ok, Data}),
            cmcore:update(Id, Data),
            {ok, State}
    end.

websocket_info(Data, #{ app := App, 
                        port := Port, 
                        id := Id, 
                        log := Log }=State) ->
    Log({ws, out, App, Port, Id, self(), Data}),
    {reply, {text, cmkit:jsone(Data)}, State}.
