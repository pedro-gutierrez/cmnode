-module(cmscheme).
-export([compile/1, render/1]).

compile(Spec) ->
    cmkit:log({cmscheme, spec, Spec}),
    cmscheme_compile:app(Spec). 

render(Ast) ->
    cmkit:log({cmscheme, ast, Ast}),
    cmscheme_render:exprs(Ast, []).
