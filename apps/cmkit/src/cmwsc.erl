-module(cmwsc).
-behaviour(gen_statem).
-export([behaviour_info/1]).
-export([start_link/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([connecting/3, connected/3, upgrading/3, ready/3, disconnected/3]).
-export([out/2]).
-record(data, {config, conn, monitor, mod}).

behaviour_info(callbacks) ->
    [{in, 1}].

out(Name, Msg) ->
    global:send(Name, {out, Msg}).

callback_mode() ->
    state_functions.

start_link(Mod, Name, Config) ->
    gen_statem:start_link({global, Name}, ?MODULE, [Config, Mod], []).

init([Config, Mod]) ->
    {Server, Port, _} = Config,
    {Conn, MRef} = connect(Server, Port),
    {ok, connecting, #data{mod=Mod, config=Config, conn=Conn, monitor=MRef},
        [{{timeout,connecting},1000,connecting}]}.

disconnected(info, Msg, Data) ->
    cmkit:log({cmsoup_client, unknown, Msg, Data}),
    {next_state, disconnected, Data}.

connecting({timeout, connecting}, _, Data) ->
    cmkit:log({cmsoup_client, timeout, connecting, Data}),
    {next_state, disconnected, Data};

connecting(info, {gun_up, _, http}, #data{conn=Conn, config={_, _, Path}}=Data) -> 
    cmkit:log({cmsoup_client, connected}),
    gun:ws_upgrade(Conn, Path),
    {next_state, upgrading, Data}.

connected({timeout, connecting}, _, Data) ->
    cmkit:log({cmsoup_client, timeout, connected, ignored}),
    {next_state, connected, Data};

connected(info, {gun_down, _, http,closed, _, _}, Data) ->
    cmkit:log({cmsoup_client, disconnected}),
    {next_state, connecting, Data}.

upgrading(info, {gun_ws_upgrade, _, ok, _}, Data) ->
    cmkit:log({cmsoup_client, upgraded}),
    {next_state, ready, Data};

upgrading(info, {gun_response, _, _, Status, _}, _) ->
    cmkit:log({cmsoup_client, upgrade_not_supported, Status}),
    {stop, ws_not_supported};

upgrading(info, {gun_error, _, _, Reason}, _) ->
    cmkit:log({cmsoup_client, upgrade_failed, Reason}),
    {stop, Reason};

upgrading(info, {gun_down, _, _, closed, _, _}, Data) ->
    cmkit:log({cmsoup_client, disconnected}),
    {next_state, connecting, Data};

upgrading({timeout, connecting}, _, Data) ->
    cmkit:log({cmsoup_client, timeout, upgrading, ignored}),
    {next_state, upgrading, Data}.

ready(info, {out, Msg}, #data{conn=Conn}=Data) -> 
    gun:ws_send(Conn, {binary, cmkit:jsone(Msg)}),
    {next_state, ready, Data};

ready(info, {gun_ws, _, {text, Raw}}, #data{mod=Mod}=Data) ->
    Mod:in(cmkit:jsond(Raw)),
    {next_state, ready, Data};

ready(info, {gun_down, _, _, closed, _, _}, Data) ->
    cmkit:log({cmsoup_client, disconnected}),
    {next_state, connecting, Data};

ready({timeout, connecting}, _, Data) ->
    cmkit:log({cmsoup_client, timeout, ready, ignored}),
    {next_state, ready, Data}.

terminate(Reason, _, #data{conn=Conn}) ->
    gun:close(Conn),
    cmkit:log({cmsoup_client, terminated, Reason}),
    ok.

connect(Server, Port) ->
    {ok, Conn} = gun:open(Server, Port),
    MRef = monitor(process, Conn),
    {Conn, MRef}.
