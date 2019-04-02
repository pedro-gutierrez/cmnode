-module(cmevents_app).
-behaviour(application).
-export([start/2, stop/1]).

%% Load all buckets configured as event
%% stores, and initialize writers for all of them
start(_StartType, _StartArgs) ->
    {ok, EStores} = cmevents_util:reload(),
    cmevents_sup:start_link(EStores).

stop(_State) ->
    ok.
