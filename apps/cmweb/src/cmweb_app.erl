-module(cmweb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    {ok, Effects} = cmconfig:effects(),
    [ open(Port, Effects) || Port <- cmconfig:ports() ],
    cmweb_sup:start_link().

stop(_) ->
    ok.

open(#{ name := Name, 
        acceptors := Acceptors,
        port := Port, 
        apps := Apps }, Effects) ->

    Dispatch = cowboy_router:compile([{'_', routes(Name, Port, Apps, Effects)}]),

    case cowboy:start_clear(Name, 
                            [{port, Port}, {num_acceptors, Acceptors}],
                            #{env => #{dispatch => Dispatch},
                              stream_handlers => [cowboy_stream_h]}) of 
        {ok, _} ->
            cmkit:log({cmweb, Name, Port, ok});
        {error, E} ->
            cmkit:warning({cmweb, Name, Port, E})
    end,
    ok;

open(_, _) -> ok.

routes(PortName, PortNumber, Apps, Effects) ->
    AppDir = atom_to_list(PortName),
    lists:flatten(lists:map(fun (App) -> 
                               app_routes(PortNumber, App, Effects)     
                            end, Apps))
    ++
    [
        {"/", cowboy_static, {priv_file, cmweb, AppDir ++ "/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, cmweb, AppDir}}
    ].

app_routes(Port, #{ name := Name, mounts := Mounts }, Effects) ->
    lists:map(fun(Mount) ->
                    mount_route(Name, Mount, Port, Effects)
              end, Mounts).

mount_route(App, #{ path := Path, transport := http }, Port, Effects) ->
    %{ Path ++ "/[...]", cmweb_http, #{app => App, port => Port, transport => http }};
    { Path, cmweb_http, #{app => App, port => Port, transport => http, effects => Effects }};

mount_route(App, #{ path := Path, transport := ws }, Port, Effects ) ->
    { Path, cmweb_ws, #{ app => App, port => Port, transport => ws, effects => Effects }}.
