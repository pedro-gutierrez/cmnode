-module(cmservice_worker).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
        ]).

start_link(App, From, Params) ->
    gen_server:start_link(?MODULE, [App, From, Params], []).

init([App, From, Params]) ->
    
    DurationMetric = cmservice_util:duration_metric_name(App),
    Pid = self(),
    Start = cmkit:micros(), 
    case cmconfig:app(App) of 
        {ok, #{ debug := Debug }=Spec} ->
            {ok, Effects} = cmconfig:effects(),
            Log = cmkit:log_fun(Debug),
            case cmcore:init(Pid, Spec, Log, Effects) of
                {ok, Model, Config} ->
                    {ok, #{ app => App,
                            start => Start,
                            spec => Spec#{ config => Config },
                            pid => Pid,
                            log => Log,
                            from => From,
                            model => Model,
                            effects => Effects,
                            duration_metric => DurationMetric,
                            data => Params}, 0};
                {error, E} ->
                    cmkit:danger({cmservice, App, init, E}),
                    {stop, normal, #{ app => App,
                                      start => Start,
                                      duration_metric => DurationMetric,
                                      status => error }}
            end;
        {error, E}->
            cmkit:danger({cmservice, App, E}),
            {stop, normal, #{ app => App, 
                              start => Start,
                              duration_metric => DurationMetric,
                              status => error }}
    end.

handle_info(timeout, #{ data := Data }=State) ->
    update(Data, maps:without([data], State));

handle_info({update, Data}, State) ->
    update(Data, State);

handle_info({terminate, #{ status := Status}=Data}, #{ app := Name,
                                                       from := From }=State) ->
    cmcore:update(From, Data#{ service => Name }),
    {stop, normal, State#{ reason => Status }};

handle_info(Data, #{ from := From }=State) ->
    cmcore:update(From, Data),
    {noreply, State}.

update(Data, #{ spec := Spec,
                from := From,
                model := Model,
                log := Log,
                effects := Effects,
                app := App}=State) -> 
    case cmcore:update(self(), Spec, Data, Model, Log, Effects) of
        {ok, Model2, Spec2} ->
            {noreply, State#{ spec => Spec2,
                              model => Model2 }};
        {error, E} ->
            cmkit:danger({App, E}),
            cmcore:update(From, E),
            {stop, normal, State#{ status => error }}
    end.

handle_call(_, _, _) -> 
    {reply, ok}.

handle_cast(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(R, #{ app := App, 
                log := Log,
                duration_metric := DurationMetric,
                start := Start} = Data) ->
    Reason = reason(Data),
    Elapsed = cmkit:elapsed(Start),
    cmmetrics:record_duration(DurationMetric, Reason, Elapsed/1000),
    Log({cmservice, App, terminate, R, Reason, Elapsed}).


reason(#{ reason := R}) -> R;
reason(_) -> error.
