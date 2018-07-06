-module(cmimg).
-export([convert/1]).

convert(#{ data := Data, 
           scale := Scale }=Spec) ->
    case eimp:identify(Data) of 
        {ok, [{type, Type},
              {width, Width},
              {height, Height}]} -> 
            eimp:convert(Data, Type, [{scale, {Width div Scale, Height div Scale}}]);
        Other -> Other
    end.



