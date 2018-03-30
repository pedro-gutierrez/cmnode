-module(cmscheme_render).
-export([exprs/2]).
-define(QUOTES, <<"\"">>).

exprs([], Out) -> {ok, cmkit:bin_join(lists:reverse(Out), <<"\n">>) };

exprs([Expr|Rem], Out) ->
    exprs(Rem, [expr(Expr)|Out]).

expr(#{ type := symbol,
        value := Value }) ->
    cmkit:bin_join([ <<"\'">>, Value]);

expr(#{ type := define,
        name := Name,
        args := Args,
        expression := Expr }) ->
    {ok, Out} = cmtemplate:render(scheme_define, #{ name => Name,
                                                    args => args(Args),
                                                    expression => expr(Expr)}),
    Out;

expr(#{ type := call,
        name := Name,
        args := Args }) ->
    
    Args2 = lists:map(fun(A) ->
                        expr(A)     
                      end, Args),
    {ok, Out} = cmtemplate:render(scheme_call, #{ name => Name,
                                                  args => Args2 }),
    Out;
        
expr(#{ type := match,
        value := V,
        expr := E }) ->
    
    {ok, Out} = cmtemplate:render(scheme_case, #{ match => expr(V),
                                                  expression => expr(E) }),
    Out;

expr(#{ type := switch,
        test := T,
        match := M }) ->
    
    {ok, Out} = cmtemplate:render(scheme_switch, #{ test => expr(T),
                                                    match => lists:map( fun expr/1, M)}),
    Out;

expr(#{ type := literal,
        value := Arg }) when is_binary(Arg) -> Arg;

expr(#{ type := string,
        value := Arg }) -> cmkit:bin_join([?QUOTES, cmkit:to_bin(Arg), ?QUOTES ]);


expr([]) ->
    <<>>.

args([]) -> <<>>;
args(Args) -> 
    cmkit:bin_join(lists:map(fun(A) ->
                      cmkit:bin_join([<<" ">>, arg(A)])
                    end, Args)).

arg(#{ type := literal, value := A}) when is_binary(A) -> A; 
arg(A) when is_binary(A) -> A.

%%tab(N) ->
%%    tab(N, []).
%%
%%tab(0, Out) -> cmkit:bin_join(Out);
%%tab(N, Out) ->
%%    tab(N-1, [<<"  ">>|Out]).
