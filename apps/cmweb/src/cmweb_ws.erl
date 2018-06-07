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
            case cmsession:new(App) of 
                {ok, #{ id := Id }=Session } ->
                    ok = cmcore:init(Spec, Session),
                    Log({ws, new_connection, App, Port, Id}),
                    {ok, State#{ id => Id, log => Log} };
                Other ->
                    cmkit:danger({ws, aborting, App, Other}),
                    {stop, Other}
            end;
       {error, E} -> 
            cmkit:log({ws, new_connection, invalid_app, App, Port, E}),
            {stop, E}
    end.

websocket_handle({binary, Data}, State) ->
    handle_data(Data, State);

websocket_handle({text, Data}, State) -> 
    handle_data(Data, State).

websocket_info(Data, #{ app := App, 
                        port := Port, 
                        id := Id, 
                        log := Log }=State) ->
    Log({ws, out, App, Port, Id, self(), Data}),
    {reply, {text, cmkit:jsone(Data)}, State}.


handle_data(<<>>, State) -> {ok, State};

handle_data(Data, #{ app := App,
                     port := Port,
                     id := Id,
                     log := Log }=State) ->

    case cmkit:jsond(Data) of
        {error, _} -> 
            Log({ws, in, App, Port, Id, invalid, Data}),
            {stop, State};
        {ok, Decoded} ->
            Log({ws, in, App, Port, Id, self(), ok, Decoded}),
            cmcore:update(Id, Decoded),
            {ok, State}
    end.
