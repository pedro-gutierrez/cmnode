-module(cmmetrics_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    load_metrics(cmmetrics:enabled()),
    cmmetrics_sup:start_link().

stop(_State) ->
    ok.


load_metrics(false) ->
    cmkit:log({cmmetrics, disabled});

load_metrics(true) ->
    lists:foreach(fun load_gauge/1, built_in_gauges()),
    lists:foreach(fun load_metrics_group/1, cmconfig:metrics()).

load_metrics_group(#{ name := Name,
                spec := Metrics }) -> 
    
    load_metrics_group(Name, maps:next(maps:iterator(Metrics))).

load_metrics_group(_, none) -> ok;
load_metrics_group(Name, {Key, Spec, I2}) ->
    load_metric(Name, Key, Spec), 
    load_metrics_group(Name, maps:next(I2)).


load_metric(Prefix, Name, #{ type := http_request_duration,
                             groups := Groups }) ->

    MetricName = metric_name(Prefix, Name),
    Res = cmmetrics:define_http_duration(MetricName, cmmetrics:http_duration_groups(Groups)),
    log_metric_status(MetricName, Res);

load_metric(Prefix, Name, #{ type := counter }) ->

    MetricName = metric_name(Prefix, Name),
    MetricHelp = metric_help(Prefix, Name),
    Res = cmmetrics:define_counter(MetricName, MetricHelp),
    log_metric_status(MetricName, Res);

load_metric(Prefix, Name, #{ type := gauge }) ->

    MetricName = metric_name(Prefix, Name),
    MetricHelp = metric_help(Prefix, Name),
    Res = cmmetrics:define_gauge(MetricName, MetricHelp),
    log_metric_status(MetricName, Res);

load_metric(Prefix, Name, Other) ->
    cmkit:warning({cmmetric, not_supported, Prefix, Name, Other}).

load_gauge(#{ name := Name, help := Help }) ->
    cmmetrics:define_gauge(Name, Help).


built_in_gauges() -> [#{ name => cmnode_cpu, help => "cmnode's average cpu usage across all processors"},
                      #{ name => cmnode_memory_used, help => "cmnode's memory usage in megabytes" },
                      #{ name => cmnode_memory_available, help => "total memory available in megabytes, as seen by cmnode" }].


metric_name(Prefix, Name) ->
    cmkit:bin_join([ cmkit:to_bin(Prefix), cmkit:to_bin(Name)], <<"_">>).


metric_help(Prefix, Name) ->
    cmkit:bin_join(lists:flatten(lists:map(fun(T) ->
                                                   cmkit:bin_split(T, <<"_">>)
                                           end, [cmkit:to_bin(Prefix), 
                                                 cmkit:to_bin(Name)])), <<" ">>).

log_metric_status(Name, Status) ->
    cmkit:log({cmmetric, cmkit:to_atom(Name), Status}).
