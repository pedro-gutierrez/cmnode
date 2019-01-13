-module(cmmetrics).
-export([enabled/0, 
         define_gauge/2,
         define_counter/2,
         define_http_duration/1, 
         define_http_duration/2,
         http_duration_groups/1,
         set/2,
         increment/2,
         record_http_duration/4,
         http_duration_name/1]).

-define(HTTP_DURATION, <<"http_request_duration">>).
-define(MS, <<"millisecond">>).


enabled() ->
    is_application_started(prometheus).


define_gauge(Name, Help) ->
    prometheus_gauge:new([{name, to_bin(Name)}, 
                          {help, Help}]).

define_counter(Name, Help) ->
    prometheus_counter:new([{name, to_bin(Name)}, 
                            {help, Help}]).


define_http_duration(Name, Buckets) ->
    prometheus_histogram:new([{name, name(Name, ?HTTP_DURATION, ?MS)},
                              {labels, [method, status]},
                              {buckets, Buckets},
                              {help, "Http Request execution time"}]).
define_http_duration(Name) ->
    define_http_duration(Name, http_duration_groups(internal)).

record_http_duration(Name, Method, Status, Value) ->
    prometheus_histogram:observe(name(Name, ?HTTP_DURATION, ?MS), [cmhttp:method(Method), Status], Value).

set(Name, Value) ->
    prometheus_gauge:set(to_bin(Name), Value).

increment(Name, Value) ->
    prometheus_counter:inc(to_bin(Name), Value).

http_duration_name(Name) ->
    name(Name, ?HTTP_DURATION, ?MS).

name(App, Kind, Unit) when is_binary(App) ->
    <<App/binary, "_", Kind/binary, "_", Unit/binary>>;

name(Name, Kind, Unit) ->
    name(to_bin(Name), Kind, Unit).


to_bin(V) -> cmkit:to_bin(V).
is_application_started(App) -> cmkit:is_application_started(App).

http_duration_groups(external) -> [50, 100, 150, 300, 500, 750, 1000];
http_duration_groups(internal) -> [0, 0.5, 1, 10, 25, 50, 100, 150, 300, 500, 1000].
