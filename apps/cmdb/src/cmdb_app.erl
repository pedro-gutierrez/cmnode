-module(cmdb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cmdb_util:reload_config(),
    Pid = cmdb_sup:start_link(),
    cmdb_util:start_buckets(),
    Pid.

stop(_State) ->
    ok.
