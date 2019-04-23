-module(cmstore_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Stores} = cmstore_util:stores(),
    cmstore_sup:start_link(Stores).

stop(_State) ->
    ok.
