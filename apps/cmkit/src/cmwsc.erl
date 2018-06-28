-module(cmwsc).
-behaviour(gen_statem).
-export([start_link/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([connecting/3, connected/3, upgrading/3, ready/3, disconnected/3]).
-export([send/2, stop/1]).

stop(Pid) ->
    gen_statem:call(Pid, stop).

send(Pid, Msg) ->
    gen_statem:call(Pid, {out, Msg}).

callback_mode() ->
    state_functions.

start_link(Name, Config, Owner) ->
    gen_statem:start_link(?MODULE, [Name, Config, Owner], []).

init([Name, #{ debug := Debug, 
               host := _,
               port := _,
               path := _} = Config, Owner]) ->
    Log = cmkit:log_fun(Debug),
    {Conn, MRef} = connect(Config),
    {ok, connecting, #{ name => Name,
                        log => Log,
                        config => Config,
                        owner => Owner,
                        conn => Conn,
                        monitor => MRef},
        [{{timeout,connecting},1000,connecting}]}.

disconnected(info, Msg, #{ log := Log, name := Name, config := Config }=Data) ->
    Log({cmwsc, Name, disconnected, Config, ignoring, Msg}),
    {next_state, disconnected, Data}.

connecting({timeout, connecting}, _, #{ log := Log, 
                                        name := Name,
                                        config := Config}=Data) ->
    Log({cmwsc, Name, connecting, Config, timeout}),
    {next_state, disconnected, Data};

connecting(info, {gun_up, _, http}, #{ conn := Conn,
                                       config := #{ path := Path }}=Data) -> 
    gun:ws_upgrade(Conn, Path),
    {next_state, upgrading, Data};

connecting(Event, Msg, #{ log := Log, 
                     name := Name,
                     config := Config}=Data) ->
    Log({cmwsc, Name, connecting, Config, ignored, Event, Msg}),
    {keep_state, Data}.

connected(info, {gun_down, _, http,closed, _, _}, Data) ->
   apply_reconnection_strategy(connected, Data); 

connected(Event, Msg, #{ log := Log, name := Name, config := Config}=Data) ->
    Log({cmwsc, Name, connected, Config, ignored, Event, Msg}),
    {next_state, connected, Data}.

upgrading(info, {gun_ws_upgrade, _, ok, _}, #{ log := Log,
                                               owner := Owner,
                                               name := Name, 
                                               config := Config}=Data) ->
    Log({cmwsc, Name, upgraded, Config}),
    Owner ! {ws, Name, up},
    {next_state, ready, Data};

upgrading(info, {gun_response, _, _, Status, _}, #{ log := Log, 
                                                    name := Name,
                                                    config := Config}) ->
    Log({cmwsc, Name, upgrade_not_supported, Config, Status}),
    {stop, ws_not_supported};

upgrading(info, {gun_error, _, _, Reason}, #{ log := Log, 
                                              name := Name,
                                              config := Config}) ->
    Log({cmwsc, Name, upgrade_failed, Config, Reason}),
    {stop, Reason};

upgrading(info, {gun_down, _, _, closed, _, _}, Data) ->
   apply_reconnection_strategy(upgrading, Data); 

upgrading(Event, Msg, #{ log := Log, 
                     name := Name,
                     config := Config}=Data) ->
    Log({cmwsc, Name, upgrading, Config, ignored, Event, Msg}),
    {next_state, upgrading, Data}.

ready({call, From}, {out, Msg}, #{ conn := Conn}=Data) -> 
    gun:ws_send(Conn, {binary, cmkit:jsone(Msg)}),
    {keep_state, Data, ok(From)};

ready(info, {gun_ws, _, {text, Raw}}, #{ owner := Owner, 
                                         name := Name }=Data) ->
    Msg = case cmkit:jsond(Raw) of 
        {ok, Term} -> Term;
        {error, E} -> E
    end,
    Owner ! {ws, Name, msg, Msg},
    {keep_state, Data};

ready(info, {gun_down, _, _, closed, _, _}, Data) ->
   apply_reconnection_strategy(ready, Data); 

ready({timeout, connecting}, _, Data) ->
    {keep_state, Data};

ready({call, From}, stop, #{ log := Log, 
                             name := Name, 
                             config := Config
                           }) ->
    Log({cmwsc, Name, ready, Config, stopping}),
    {stop_and_reply, normal, ok(From)};

ready(Event, Msg, #{ log := Log,
                 name := Name,
                 config := Config}=Data) ->
    Log({cmwsc, Name, ready, Config, ignored, Event, Msg}),
    {next_state, ready, Data}.

terminate(Reason, _, #{ log := Log, name := Name, config := Config, conn := Conn}) ->
    gun:close(Conn),
    Log({cmwsc, Name, terminated, Config, Reason}),
    ok.

connect(#{ host := Host,
           port := Port}) ->
    {ok, Conn} = gun:open(cmkit:to_list(Host), Port),
    MRef = monitor(process, Conn),
    {Conn, MRef}.

ok(From) -> [{reply, From, ok}].

apply_reconnection_strategy(State, #{ log := Log, 
                                      owner := Owner,
                                      name := Name,
                                      config := Config }=Data ) ->
    case maps:get(persistent, Config, true) of 
        true -> 
            Log({cmwsc, Name, State, Config, closed, reconnecting}),
            {next_state, connecting, Data};
        false ->
            Owner ! {ws, Name, down},
            Log({cmwsc, Name, State, Config, closed, terminating}),
            {stop, normal}
    end.
