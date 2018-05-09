-module(cmscheme).
-export([compile/1, render/1]).

compile(Spec) ->
    cmscheme_compile:app(Spec). 

render(Ast) ->
    cmscheme_render:exprs(Ast, []).
