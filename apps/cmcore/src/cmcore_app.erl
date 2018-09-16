-module(cmcore_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pg2:create(contexts),
    cmcore_sup:start_link().

stop(_State) ->
    ok.
