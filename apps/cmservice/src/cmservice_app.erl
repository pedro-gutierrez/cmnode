-module(cmservice_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    instrument_services(),
    cmservice_sup:start_link().

stop(_State) ->
    ok.

instrument_services() ->
    Services = cmconfig:services(),
    lists:foreach(fun(#{ name := Name}) ->
                          register_duration_metric(Name)
                  end, Services).

register_duration_metric(Service) ->
    DurationMetric = cmservice_util:duration_metric_name(Service),
    cmmetrics:define_duration(DurationMetric).
