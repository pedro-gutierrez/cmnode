-module(cmencode).
-export([encode/1]).

encode(#{ type := keyword, value := Data}) when is_atom(Data) ->
    {ok, Data}.
