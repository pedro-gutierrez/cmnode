-module(cmweb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    [ open(Port) || Port <- cmconfig:ports() ],
    cmweb_sup:start_link().

stop(_) ->
    ok.

open(#{ name := Name, 
        acceptors := Acceptors,
        port := Port, 
        apps := Apps }) ->

    Dispatch = cowboy_router:compile([{'_', routes(Name, Port, Apps)}]),
    {ok, _} = cowboy:start_clear(Name, 
                                 [{port, Port}, {num_acceptors, Acceptors}],
                                 #{env => #{dispatch => Dispatch},
                                  stream_handlers => [cowboy_stream_h]}),
    cmkit:log({cmweb, Name, Port, ok});

open(_) -> ok.

routes(PortName, PortNumber, Apps) ->
    AppDir = atom_to_list(PortName),
    lists:flatten(lists:map(fun (App) -> 
                               app_routes(PortNumber, App)     
                            end, Apps))
    ++
    [
        {"/", cowboy_static, {priv_file, cmweb, AppDir ++ "/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, cmweb, AppDir}}
    ].

app_routes(Port, #{ name := Name, mounts := Mounts }) ->
    lists:map(fun(Mount) ->
                    mount_route(Name, Mount, Port)
              end, Mounts).

mount_route(App, #{ path := Path, transport := http }, Port) ->
    %{ Path ++ "/[...]", cmweb_http, #{app => App, port => Port, transport => http }};
    { Path, cmweb_http, #{app => App, port => Port, transport => http }};

mount_route(App, #{ path := Path, transport := ws }, Port) ->
    { Path, cmweb_ws, #{ app => App, port => Port, transport => ws }}.
