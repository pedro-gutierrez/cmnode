-module(cmdb2_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Buckets} = cmdb2_util:reload(),
    cmdb2_sup:start_link(Buckets).

stop(_State) ->
    ok.
