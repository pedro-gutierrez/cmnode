-module(cmconfig_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cmeffect:reload(),
    Pid = cmconfig_sup:start_link(),
    cmconfig_compiler_sup:start(),
    Pid.

stop(_State) ->
    ok.
