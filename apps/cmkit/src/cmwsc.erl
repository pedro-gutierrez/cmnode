-module(cmwsc).
-behaviour(gen_statem).
-export([start_link/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([connecting/3, connected/3, upgrading/3, ready/3, disconnected/3]).
-export([send/2, stop/1]).

stop(Pid) ->
    check_and_send(Pid, stop).

send(Pid, Msg) ->
    check_and_send(Pid, {out, Msg}).

check_and_send(Pid, Msg) ->
    case erlang:is_process_alive(Pid) of
        false ->
                cmkit:log({cmwcs, Pid, dead, cannot_send, Msg}),
                process_dead;
        true ->
            gen_statem:call(Pid, Msg)
    end.

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
                        monitor => MRef,
                        state => #{} },
        [{{timeout,connecting},1000,connecting}]}.

disconnected(info, Msg, #{ log := Log, name := Name, config := Config }=Data) ->
    Log({cmwsc, Name, disconnected, Config, ignoring, Msg}),
    {next_state, disconnected, Data};

disconnected(From, stop, Data) -> 
    handle_stop(From, Data).

connecting(From, stop, Data) -> 
    handle_stop(From, Data);

connecting({timeout, connecting}, _, #{ log := Log, 
                                        name := Name,
                                        config := Config}=Data) ->
    Log({cmwsc, Name, connecting, Config, timeout}),
    {next_state, disconnected, Data};

connecting(info, {gun_up, _, _}, #{ log :=Log,
                                    name := Name,
                                    conn := Conn,
                                    config := #{ path := Path }}=Data) -> 
    Log({cmwsc, Name, connected, upgrading, Path}),
    gun:ws_upgrade(Conn, Path),
    {next_state, upgrading, Data};

connecting(Event, Msg, #{ log := Log, 
                     name := Name,
                     config := Config}=Data) ->
    Log({cmwsc, Name, connecting, Config, ignored, Event, Msg}),
    {keep_state, Data}.

connected(From, stop, Data) -> 
    handle_stop(From, Data);

connected(info, {gun_down, _, _, closed, _, _}, Data) ->
   apply_reconnection_strategy(connected, Data); 

connected(Event, Msg, #{ log := Log, name := Name, config := Config}=Data) ->
    Log({cmwsc, Name, connected, Config, ignored, Event, Msg}),
    {next_state, connected, Data}.

upgrading(info, {gun_upgrade, _, _, _, _}, #{ log := Log,
                                               owner := Owner,
                                               name := Name, 
                                               config := Config}=Data) ->
    Log({cmwsc, Name, upgraded, Config}),
    Owner ! {ws, Name, up},
    {next_state, ready, Data};

upgrading(info, {gun_response, _, _, _, Status, Headers}, #{ log := Log, 
                                                             name := Name,
                                                             owner := Owner } = Data) ->
    case Status of 
        200 ->
            {keep_state, Data};
        _ ->
            Msg = #{ status => Status,
                      headers => maps:from_list(Headers) },
            Owner ! {ws, Name, msg, Msg},
            Log({cmwsc, Name, upgrading, Msg, will_stop}),
            {stop, normal}
    end;

upgrading(info, {gun_error, _, _, Reason}, #{ log := Log, 
                                              name := Name,
                                              config := Config}) ->
    Log({cmwsc, Name, upgrade_failed, Config, Reason}),
    {stop, Reason};

upgrading(info, {gun_down, _, _, closed, _, _}, Data) ->
   apply_reconnection_strategy(upgrading, Data); 

upgrading(From, stop, Data) -> 
    handle_stop(From, Data);

upgrading(Event, Msg, #{ log := Log, 
                     name := Name,
                     config := Config}=Data) ->
    Log({cmwsc, Name, upgrading, Config, ignored, Event, Msg}),
    {next_state, upgrading, Data}.

ready({call, From}, {out, Msg}, Data) -> 
    ws_send(Msg, Data),
    {keep_state, Data, ok(From)};

ready(info, {gun_ws, _, _, {text, Raw}}, #{ log := Log,
                                            owner := Owner,
                                            name := Name }=Data) ->
    Msg = case cmkit:jsond(Raw) of 
        {ok, Term} -> Term;
        {error, E} -> E
    end,
    
    Log({cmwsc, Name, in, {decoded, Msg}, {raw, Raw}}),
    
    apply_protocol(Msg, Data),

    Owner ! {ws, Name, msg, Msg},
    {keep_state, Data};

ready(info, {gun_ws, _, _, {close, _, _}}, Data) ->
   apply_reconnection_strategy(ready, Data); 

ready({timeout, connecting}, _, Data) ->
    {keep_state, Data};

ready(From, stop, Data) -> 
    handle_stop(From, Data);


ready(Event, Msg, #{ log := Log,
                 name := Name,
                 config := Config}=Data) ->
    Log({cmwsc, Name, ready, Config, ignored, Event, Msg}),
    {next_state, ready, Data}.

handle_stop({call, From}, #{ log := Log, 
                                   name := Name, 
                                   config := Config
                                 }) ->
    Log({cmwsc, Name, ready, Config, stopping}),
    {stop_and_reply, normal, ok(From)}.

terminate(Reason, _, #{ log := Log, name := Name, config := Config, conn := Conn}) ->
    ok = gun:close(Conn),
    Log({cmwsc, Name, terminated, Config, Reason}),
    ok.

connect(#{ host := Host,
           port := Port}) ->
    {ok, Conn} = gun:open(cmkit:to_list(Host), Port, #{protocols => [http]}),
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

apply_protocol(In, #{ 
                      name := Name,
                      config := #{ protocol := #{ spec := #{ decoders := Decoders, 
                                                             update := Update,
                                                             encoders := Encoders }}},
                      state := State } = Data) ->
    
    case try_decoders(Decoders, In) of 
        {ok, {Msg, Decoded}} ->
            case maps:get(Msg, Update, undef) of 
                undef -> 
                    cmkit:log({cmwsc, Name, protocol, missing_update, Msg}); 
                Clauses ->
                    Ctx = #{ model => State, data => Decoded },
                    case select_clause(Clauses, Ctx) of 
                        {ok, #{ model := ModelSpec,
                                cmds := Cmds }} ->
                            case cmencode:encode(ModelSpec, Decoded) of 
                                {ok, ModelUpdates} -> 
                                    State2 = maps:merge(State, ModelUpdates),
                                    case apply_cmds(Cmds, Encoders, Data#{ state => State2 }) of 
                                        ok -> ok;
                                        Other ->
                                            cmkit:warning({cmwsc, Name, protocol, cmd_error, Other})
                                    end;
                                Other ->
                                    cmkit:warning({cmwsc, Name, protocol, update_error, ModelSpec, Decoded, Other})
                            end;
                        Other -> 
                            cmkit:warning({cmwsc, Name, protocol, no_update_clause_matching, Clauses, Ctx, Other})
                    end
            end;
        no_match -> 
            ok
    end;

apply_protocol(_, Data) -> 
    cmkit:log({cmwsc, no_protocol, Data}),
    ok.

try_decoders([], _) -> no_match;
try_decoders([#{ msg := Msg,
                 spec := Spec}|Rem], Data) ->
    case cmdecode:decode(Spec, Data) of 
        {ok, Decoded} ->
            {ok, {Msg, Decoded}};
        _ ->
            try_decoders(Rem, Data)
    end.

select_clause([], _) -> none;
select_clause([#{ condition := Cond }=Spec|Rem], Ctx) -> 
    case cmeval:eval(Cond, Ctx) of 
        true -> {ok, Spec};
        false -> 
            select_clause(Rem, Ctx)
    end.

apply_cmds([], _, _) -> ok;
apply_cmds([#{ effect := _,
               encoder := Encoder }|Cmd], Encoders, #{ name := Name, 
                                                       state := State }=Data) -> 
    case maps:get(Encoder, Encoders, undef) of 
        undef -> 
            cmkit:warning({cmwsc, Name, cmd, Cmd, skipped, no_such_encoder, Encoders});
        EncoderSpec -> 
            case cmencode:encode(EncoderSpec, State) of 
                {ok, Encoded} -> 
                    ws_send(Encoded, Data);
                Other -> 
                    cmkit:warning({cmwsc, Name, cmd, Cmd, skipped, encode_error, Other})
            end
    end.

ws_send(Msg, #{ name := Name,
                log := Log,
                conn := Conn}) -> 
    Raw = cmkit:jsone(Msg),
    Log({cmwsc, Name, out, {encoded, Msg}, {raw, Raw}}),
    gun:ws_send(Conn, {text, Raw}).
