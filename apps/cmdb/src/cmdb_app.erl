-module(cmdb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Buckets} = cmdb_util:reload(),
    cmdb_sup:start_link(Buckets).

stop(_State) ->
    ok.
