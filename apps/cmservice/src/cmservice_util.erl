-module(cmservice_util).
-export([duration_metric_name/1]).

duration_metric_name(Service) ->
    <<(cmkit:to_bin(Service))/binary, "_service_duration">>.
