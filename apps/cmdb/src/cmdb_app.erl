-module(cmdb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Pid = cmdb_sup:start_link(),
    cmdb_compiler_sup:start(),
    Pid.

stop(_State) ->
    ok.
