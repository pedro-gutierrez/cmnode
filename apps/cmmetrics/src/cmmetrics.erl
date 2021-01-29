-module(cmmetrics).
-export([
         define_gauge/2,
         define_counter/2,
         define_http_duration/1, 
         define_http_duration/2,
         define_http_count/1, 
         define_ws_duration/1, 
         define_ws_count/1, 
         define_duration/1,
         http_duration_groups/0,
         ws_duration_groups/0,
         set/2,
         increment/2,
         increment_http_count/1,
         decrement_http_count/1,
         increment_ws_count/1,
         decrement_ws_count/1,
         record_http_duration/4,
         record_ws_duration/3,
         record_duration/3
        ]).
-define(HISTOGRAM_TABLE, prometheus_histogram_table).
-define(COUNTER_TABLE, prometheus_counter_table).
-define(GAUGE_TABLE, prometheus_gauge_table).

define_gauge(Name, Help) ->
    MetricName = to_bin(Name),
    prometheus_gauge:declare([{name, MetricName}, 
                              {help, Help}]),
    {ok, MetricName}.

define_counter(Name, Help) ->
    MetricName = to_bin(Name),
    prometheus_counter:declare([{name, MetricName}, 
                                {help, Help}]),
    {ok, MetricName}.

define_histogram(Name, Labels, Buckets, Help) ->
    MetricName = to_bin(Name), 
    prometheus_histogram:declare([{name, MetricName},
                                  {labels, Labels},
                                  {buckets, Buckets},
                                  {help, Help}]),
    {ok, MetricName}.

define_http_duration(Name, Buckets) ->
    define_histogram(Name, [method, status], Buckets, "Http Request execution time in milliseconds").

define_http_duration(Name) ->
    define_http_duration(Name, http_duration_groups()).

define_ws_duration(Name, Buckets) ->
    define_histogram(Name, [termination], Buckets, "Websocket lifetime in seconds").

define_ws_duration(Name) ->
    define_ws_duration(Name, ws_duration_groups()).

define_duration(Name) ->
    define_histogram(Name, [status], http_duration_groups(), "Duration in milliseconds").

define_http_count(Name) ->
    define_gauge(Name, "Active Http requests").

define_ws_count(Name) ->
    define_gauge(Name, "Active Websocket connections").

record_http_duration(Name, Method, Status, Value) ->
    spawn(fun() ->
                  prometheus_histogram:observe(Name, [cmhttp:method(Method), Status], Value)
          end).

record_ws_duration(Name, Reason, Value) ->
    spawn(fun() ->
                  prometheus_histogram:observe(Name, [Reason], Value)
          end).

record_duration(Name, Reason, Value) ->
    spawn(fun() ->
                  prometheus_histogram:observe(Name, [Reason], Value)
          end).

set(Name, Value) ->
    spawn(fun() ->        
                  prometheus_gauge:set(Name, Value)
          end).

increment(Name, Value) ->
    spawn(fun() -> 
                  prometheus_counter:inc(Name, Value)
          end).

increment_ws_count(Name) ->
    spawn(fun() ->
                  prometheus_gauge:inc(Name)
          end).

decrement_ws_count(Name) ->
    spawn(fun() ->
                  prometheus_gauge:dec(Name)
          end).

increment_http_count(Name) ->
    spawn(fun() ->
                  prometheus_gauge:inc(Name)
          end).

decrement_http_count(Name) ->
    spawn(fun() ->
                  prometheus_gauge:dec(Name)
          end).

to_bin(V) -> cmkit:to_bin(V).

ws_duration_groups() -> [30, 60, 300, 600, 1800, 3600].
http_duration_groups() -> [0.5, 1, 10, 25, 50, 100, 150, 300, 500, 1000, 2500, 5000].
