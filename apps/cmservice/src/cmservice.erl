-module(cmservice).
-export([run/2]).

run(App, Data) ->
    case cmservice_sup:new(App, Data) of 
        {ok, _} -> ok;
        Other -> Other
    end.
