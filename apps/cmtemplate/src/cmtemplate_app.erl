-module(cmtemplate_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cmtemplate_util:reload(),
    cmtemplate_sup:start_link().

stop(_State) ->
    ok.
