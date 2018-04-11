-module(cmweb_ws).
-export([
         init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(Req, State) ->
    {cowboy_websocket, Req, State }.

websocket_init(#{app := App }=State) ->
    {ok, #{ id := Id }=Session } = cmsession:new(App),
    ok = cmcore:init(#{}, Session),
    cmkit:log({ws, new_connection, App, Id}),
    {ok, State#{ id => Id} }.

websocket_handle({text, Text}, #{ app := App, 
                                  id := Id }=State) ->
    case cmkit:jsond(Text) of
        {error, _} -> 
            cmkit:log({ws, in, App, Id, invalid, Text}),
            {stop, State};
        {ok, Data} ->
            cmkit:log({ws, in, App, Id, self(), ok, Data}),
            cmcore:update(Id, Data),
            {ok, State}
    end.

websocket_info(Data, #{ app := App, id := Id}=State) ->
    cmkit:log({ws, reply, App, Id, self(), Data}),
    {reply, {text, cmkit:jsone(Data)}, State}.
