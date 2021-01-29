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
                                                %AppDir = atom_to_list(PortName),
    lists:flatten(lists:map(fun (App) -> 
                                    port_routes(PortNumber, App, Effects)     
                            end, Apps))
        ++
        [
                                                %{"/", cowboy_static, {priv_file, cmweb, AppDir ++ "/index.html"}},
         {"/[...]", cowboy_static, {dir, cmkit:asset(PortName)}}
        ].

port_routes(Port, #{ name := Name, mounts := Mounts }, Effects) ->
    port_routes_with_metrics( lists:map(fun(Mount) ->
                                                mount_route(Name, Mount, Port, Effects)
                                        end, Mounts)).

mount_route(App, #{ path := Path, transport := Transport }, Port, Effects) ->
    State = with_instruments(#{ app => App,
                                port => Port,
                                transport => Transport,
                                effects => Effects }),
    {Path, handler(Transport), State}.

handler(http) -> cmweb_http;
handler(ws) -> cmweb_ws.


port_routes_with_metrics(Mounts) ->
    [{"/metrics", cmweb_http_metrics, none}|Mounts].


with_instruments(#{ transport := Transport,
                    app := App }=State) ->
    {IncrFun, DurFun, DecrFun} = instruments(App, Transport),
    State#{ instruments => #{ increment => IncrFun,
                              duration => DurFun,
                              decrement => DecrFun }}.

instruments(App, http) ->
    {ok, DMetric} = cmmetrics:define_http_duration(http_duration_metric_name(App)),
    {ok, CMetric} = cmmetrics:define_http_count(http_count_metric_name(App)),
    { fun() -> cmmetrics:increment_http_count(CMetric) end,
      fun(M, S, D) -> cmmetrics:record_http_duration(DMetric, M, S, D) end,
      fun() -> cmmetrics:decrement_http_count(CMetric) end 
    };

instruments(App, ws) ->
    {ok, DMetric} = cmmetrics:define_ws_duration(ws_duration_metric_name(App)),
    {ok, CMetric} = cmmetrics:define_ws_count(ws_count_metric_name(App)),
    { fun() -> cmmetrics:increment_ws_count(CMetric) end,
      fun(R, D) -> cmmetrics:record_ws_duration(DMetric, R, D) end,
      fun() -> cmmetrics:decrement_ws_count(CMetric) end 
    }.


http_duration_metric_name(App) ->
    <<(cmkit:to_bin(App))/binary, "_http_request_duration">>.

http_count_metric_name(App) ->
    <<(cmkit:to_bin(App))/binary, "_http_active_requests">>.

ws_duration_metric_name(App) ->
    <<(cmkit:to_bin(App))/binary, "_ws_connection_duration">>.

ws_count_metric_name(App) ->
    <<(cmkit:to_bin(App))/binary, "_ws_active_connections">>.



