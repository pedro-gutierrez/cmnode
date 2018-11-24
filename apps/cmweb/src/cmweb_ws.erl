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
            Pid = self(),
            Log = cmkit:log_fun(Debug),
            {ok, Model, Config} = cmcore:init(Pid, Spec, Log),
            Spec2 = Spec#{ config => Config },
            Log({ws, new_connection, App, Port, Pid}),
            {ok, State#{ spec => Spec2, model => Model, log => Log} };
       {error, E} -> 
            cmkit:warning({ws, new_connection, no_such_app, App, Port, E}),
            {stop, E}
    end.

websocket_handle({binary, Data}, State) ->
    handle_data(Data, State);

websocket_handle({text, Data}, State) -> 
    handle_data(Data, State).


websocket_info(terminate, #{ app := App,
                             port := Port,
                             log := Log }=State) ->
    Log({App, Port, self(), terminate}),
    {stop, State};

websocket_info({update, Data}, #{ model := Model,
                                  spec := Spec,
                                  log := Log }=State) ->

    {ok, Model2} = cmcore:update(self(), Spec, Data, Model, Log),
    {ok, State#{ model => Model2 }};

websocket_info(Data, #{ app := App, 
                        port := Port, 
                        log := Log }=State) ->
    Log({App, Port, self(), out, Data}),
    {reply, {text, cmkit:jsone(Data)}, State}.


handle_data(<<>>, State) -> {ok, State};

handle_data(Data, #{ app := App,
                     port := Port,
                     spec := Spec,
                     model := Model,
                     log := Log }=State) ->

    case cmkit:jsond(Data) of
        {error, _} -> 
            Log({ws, in, App, Port, self(), invalid, Data}),
            {stop, State};
        {ok, Decoded} ->
            Log({App, Port, self(), in, Decoded}),
            {ok, Model2} = cmcore:update(self(), Spec, Decoded, Model, Log),
            {ok, State#{ model => Model2 }}
    end.
