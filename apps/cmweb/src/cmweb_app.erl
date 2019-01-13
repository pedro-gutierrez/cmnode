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
                               port_routes(PortNumber, App, Effects)     
                            end, Apps))
    ++
    [
        {"/", cowboy_static, {priv_file, cmweb, AppDir ++ "/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, cmweb, AppDir}}
    ].

port_routes(Port, #{ name := Name, mounts := Mounts }, Effects) ->
    port_routes_with_metrics(
      lists:map(fun(Mount) ->
                        mount_route(Name, Mount, Port, Effects)
                end, Mounts), cmmetrics:enabled()).

mount_route(App, #{ path := Path, transport := http }, Port, Effects) ->
    mount_route_with_metrics({ Path, cmweb_http, #{app => App, 
                                                   port => Port, 
                                                   transport => http, 
                                                   effects => Effects}}, cmmetrics:enabled());
    
mount_route(App, #{ path := Path, transport := ws }, Port, Effects ) ->
    { Path, cmweb_ws, #{ app => App, port => Port, transport => ws, effects => Effects }}.


port_routes_with_metrics(Mounts, false) -> Mounts;
port_routes_with_metrics(Mounts, true) ->
    [{"/metrics", cmweb_http_metrics, none}|Mounts].


mount_route_with_metrics(R, false) -> R;
mount_route_with_metrics({Path, Handler, #{ app := App }=State}, true) ->
    cmmetrics:define_http_duration(App),
    RecordFun = fun(App0, Method, Code, Elapsed) ->
                        cmmetrics:record_http_duration(cmkit:to_bin(App0), 
                                                        Method, Code, Elapsed)
                end,
    {Path, Handler, State#{ record_metric_fun => RecordFun }}.







