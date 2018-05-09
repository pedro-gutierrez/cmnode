-module(cmconfig_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = cmconfig_sup:start_link(),
    ok = cmconfig_loader:load(),
    {ok, Pid}.

stop(_State) ->
    ok.
