-module(cmcode).

-export([load_beams/2, compile/1]).

load_beams(App, Prefix) ->
    Dir = code:lib_dir(App, ebin),
    Files = cmkit:files(Dir, ".beam"),
    Mods = lists:foldl(fun(F, Mods) ->
                               ModName = filename:rootname(filename:basename(F)),
                               case cmkit:prefix(ModName, Prefix) of
                                   no_match -> Mods;
                                   _ ->
                                       ModFile = Dir ++ "/" ++ ModName,
                                       Mod = cmkit:to_atom(ModName),
                                       code:purge(Mod),
                                       code:load_abs(ModFile),
                                       [Mod|Mods]
                               end
                       end, [], Files),
    Mods.

compile(#{ module := Module,
           functions := Functions }) ->

    ModForm = erl_syntax:revert(
                erl_syntax:attribute(
                  erl_syntax:atom(module),[erl_syntax:atom(Module)])),


    ExportForm = erl_syntax:revert(
                   erl_syntax:attribute(
                     erl_syntax:atom(export),
                     [erl_syntax:list([ 
                                        erl_syntax:arity_qualifier(
                                          erl_syntax:atom(Fun),
                                          erl_syntax:integer(maps:get(arity, maps:get(Fun, Functions)))) ||
                                          Fun <- maps:keys(Functions) ])])),

    FunctionForms = [ erl_syntax:revert(
                        erl_syntax:function(
                          erl_syntax:atom(Fun), [ compile(C) || C <- maps:get(clauses, maps:get(Fun, Functions)) ])) 
                      || Fun <- maps:keys(Functions) ],

    case compile:forms([ModForm, ExportForm] ++ FunctionForms) of 
        {ok, Mod, Bin} -> 
            code:load_binary(Mod, [], Bin);
        Other ->
            cmkit:danger({code, Other})
    end;

compile(#{ vars := Vars,
           body := Body }) ->

    erl_syntax:clause([ compile(V) || V <- Vars ], [], [compile(Body)]);

compile(#{ var := V }) ->
    erl_syntax:variable(cmkit:to_list(V));

compile(#{ atom := A }) ->
    erl_syntax:atom(A);

compile(#{ abstract := A}) ->
    erl_syntax:abstract(A);

compile(#{ tuple := A}) ->
    erl_syntax:tuple(compile(A));

compile(#{ application := #{ module := M,
                             function := F,
                             args := A}}) ->
    erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(M), erl_syntax:atom(F)), compile(A));

compile(#{ application := #{ function := F,
                             args := A}}) ->
    erl_syntax:application(erl_syntax:atom(F), compile(A));

compile(underscore) ->
    erl_syntax:underscore();

compile(A) when is_atom(A) ->
    erl_syntax:atom(A);

compile(Items) when is_list(Items) ->
    lists:map(fun compile/1, Items).
