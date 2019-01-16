-module(cmservice).
-export([run/2, run/3]).


run(App, Data) ->
    run(App, self(), Data).

run(App, From, Data) ->
    cmservice_sup:new(App, From, Data).
