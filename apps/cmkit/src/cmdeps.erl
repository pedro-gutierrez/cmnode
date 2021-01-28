-module(cmdeps).
-export([main/1]).

main(Loc) ->
    Libs = filename:join([Loc, "_build/default/lib/*/ebin/*.app"]),
    Plugins = filename:join([Loc, "_build/default/plugins/*/ebin/*.app"]),
    AppFiles = filelib:wildcard(Libs) ++ filelib:wildcard(Plugins),
                                                %io:format("App files found in ~p: ~p~n", [Paths, AppFiles]),
    Deps = [{App, proplists:get_value(applications, Props, []), apptype(Props)}
            || {ok, [{_,App,Props}]} <-
                   [file:consult(AppFile) || AppFile <- AppFiles]],
    to_graphviz(Deps).

apptype(Props) ->
    case proplists:get_value(mod, Props) of
        undefined -> library;
        _ -> regular
    end.

to_graphviz(Deps) ->
    io:format("Deps: ~p~n", [Deps]),
    Bytes = ["digraph G { ",
             "K=0.25; ",
             "ratio=0.75; ",
             "overlap=\"9:prism\"; ",
             [io_lib:format("~p [shape=box] ", [App])
              || {App, _, library} <- Deps],
             [[io_lib:format("~p->~p ", [App,Dep]) || Dep <- DepList -- [kernel, stdlib]]
              || {App, DepList, _} <- Deps],
             "}"],
    file:write_file("app-deps.dot", Bytes),
    io:format("~s",[os:cmd("dot app-deps.dot -Tpng -o app-deps.png")]).
