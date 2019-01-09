-module(cmcluster).
-export([state/0]).

state() ->
    {ok, V} = cmkit:app_env(cmcluster, state),
    V.
