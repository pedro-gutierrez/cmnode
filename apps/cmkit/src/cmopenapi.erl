-module(cmopenapi).
-export([spec/1]).


spec(_Spec) -> 

    Paths = #{},
    Schemas = #{},

    {ok, #{ paths => Paths, 
            components => #{ schemas => Schemas } }}.
