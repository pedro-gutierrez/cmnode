-module(cmweb_ws).
-export([
         init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).
init(Req, State) ->
    {cowboy_websocket, Req, State#{ req => Req }}.

websocket_init(#{ app := App, 
                  req := Req,
                  port := Port, 
                  effects := Effects,
                  instruments := #{ increment := IncrFun }}=State) ->
    IncrFun(),
    Start = cmkit:micros(),
    case cmconfig:app(App) of
        {ok, #{ debug := Debug }=Spec} -> 
            Pid = self(),
            Log = cmkit:log_fun(Debug),
            {ok, Effects} = cmconfig:effects(),
            {ok, Model, Config} = cmcore:init(Pid, Spec, Log, Effects),
            Spec2 = Spec#{ config => Config },
            Log({ws, new, App, Port, Pid}),
            case apply_filters(Spec2, Model, Req, Log, Effects) of 
                {ok, Model2, Spec3} ->
                    {ok, State#{ start => Start,
                                 spec => Spec3, 
                                 model => Model2, 
                                 log => Log }};
                {error, E} ->
                    cmkit:danger({ws, filters, App, Port, E}),
                    stop(State#{ start => Start,
                                 log => Log,
                                 model => Model,
                                 spec => Spec2}, E)
            end;
        {error, E} -> 
            cmkit:warning({ws, new, unknown_app, App, Port, E}),
            stop(State#{ start => Start }, E)
    end.

apply_filters(#{ filters := [_|_]}=Spec, Model, Req, Log, Effects) ->
    Data = #{ headers => cowboy_req:headers(Req) },
    cmcore:update(self(), Spec#{ filters_only => true }, Data, Model, Log, Effects);

apply_filters(Spec, Model, _, _, _) ->
    {ok, Model, Spec}.

websocket_handle({binary, Data}, State) ->
    handle_data(Data, State);

websocket_handle({text, Data}, State) -> 
    handle_data(Data, State).

websocket_info({terminate = R, _}, State) -> 
    stop(State, R);


websocket_info({update, Data}, #{ app := App,
                                  port := Port,
                                  model := Model,
                                  spec := Spec,
                                  log := Log,
                                  effects := Effects }=State) ->

    case cmcore:update(self(), Spec, Data, Model, Log, Effects) of 
        {ok, Model2, Spec2} ->
            {ok, State#{ spec => Spec2,
                         model => Model2 }};
        {error, E} ->
            cmkit:danger({App, Port, self(), E}),
            stop(State, E)
    end;

websocket_info(Data, #{ app := App, 
                        port := Port, 
                        log := Log }=State) ->
    Log({ws, out, App, Port, self(), Data}),
    {reply, {text, cmkit:jsone(Data)}, State}.


stop(#{ start := Start,
        app := App, 
        log := Log, 
        port := Port,
        instruments := #{ duration := DurationFun,
                          decrement := DecrFun }} = State, Reason) ->

    Elapsed = cmkit:elapsed(Start),
    Log({App, Port, self(), Elapsed, stop}),
    DurationFun(reason(Reason), trunc(Elapsed/1000000)),
    DecrFun(),
    {stop, State}.

reason(terminate) -> normal;
reason(_) -> error.


handle_data(<<>>, State) -> {ok, State};

handle_data(Data, #{ app := App,
                     port := Port,
                     spec := Spec,
                     model := Model,
                     log := Log,
                     effects := Effects }=State) ->

    case cmkit:jsond(Data) of
        {error, _} -> 
            Log({ws, in, App, Port, self(), invalid, Data}),
            {ok, State};
        {ok, Decoded} ->
            Log({ws, in, App, Port, self(), Decoded}),
            case cmcore:update(self(), Spec, #{ effect => web,
                                                data => Decoded }, Model, Log, Effects) of 
                {ok, Model2, Spec2} ->
                    {ok, State#{ spec => Spec2,
                                 model => Model2 }};
                {error, E} ->
                    Log({ws, App, Port, self(), E}),
                    {ok, State}
            end
    end.
