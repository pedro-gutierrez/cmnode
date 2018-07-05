-module(cmscheme).
-export([compile/1, compile/2, render/1]).

compile(Spec) -> compile(Spec,#{}).

compile(Spec, Settings) ->
    cmscheme_compile:app(Spec, Settings). 

render(Ast) ->
    cmscheme_render:exprs(Ast, []).
