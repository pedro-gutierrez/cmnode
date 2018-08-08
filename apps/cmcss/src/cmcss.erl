-module(cmcss).
-export([compile/2]).


compile(Spec, Settings) -> 
    cmkit:warning({cmcss, Spec, Settings}),
    {ok, <<"body { background: green; }">>}.

