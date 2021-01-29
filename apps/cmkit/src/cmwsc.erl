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
            process_dead;
        true ->
            gen_statem:call(Pid, Msg)
    end.

callback_mode() ->
    state_functions.

start_link(Name, Config, Owner) ->
    gen_statem:start_link(?MODULE, [Name, Config, Owner], []).

init([Name, #{ debug := Debug, 
               timeout := Timeout,
               url := Url,
               host := _,
               port := _,
               path := _} = Config, Owner]) ->
    Log = cmkit:log_fun(Debug),
    Log({cmwsc, Name, starting, Url, Timeout}),
    Data = connect(#{ name => Name,
                      timeout => Timeout,
                      log => Log,
                      config => Config,
                      owner => Owner,
                      buffer => queue:new(),
                      state => #{}}),
    {ok, connecting, Data, 
     [{{timeout,connecting}, Timeout ,connecting}]}.

disconnected(info, {gun_down, _, _}, Data) ->
    {keep_state, Data};

disconnected(info, {gun_up, Conn, _}, #{ log := Log, 
                                         name := Name, 
                                         config := #{ path := Path} = Config 
                                       }=Data) ->
    Data2 = maybe_monitor(Conn, Data),
    Res = upgrade(Conn, Path, Config),
    Log({cmwsc, Name, disconnected, upgrading, Path, Res}),
    {next_state, upgrading, Data2};

disconnected({call, _}, {out, Msg}, Data) ->
    Data2 = buffer(Msg, disconnected, Data),
    {keep_state, Data2};

disconnected(From, stop, Data) -> 
    handle_stop(From, Data);

disconnected(Ev, Other, #{ log := Log,
                           name := Name }=Data) ->
    Log({cmwsc, Name, disconnected, ignored, Ev, Other}),
    {keep_state, Data}.

connecting(From, stop, Data) -> 
    handle_stop(From, Data);

connecting({timeout, connecting}, _, #{ name := Name}=Data) ->
    cmkit:warning({cmwsc, Name, connecting, timeout}),
    {next_state, disconnected, Data};

connecting(info, {gun_up, _, _}, #{ log :=Log,
                                    name := Name,
                                    conn := Conn,
                                    config := #{ path := Path } = Config}=Data) -> 
    Res = upgrade(Conn, Path, Config),
    Log({cmwsc, Name, connecting, upgrading, Path, Res}),
    {next_state, upgrading, Data};

connecting({call, _}, {out, Msg}, Data) ->
    Data2 = buffer(Msg, connecting, Data),
    {keep_state, Data2};

connecting(From, stop, Data) -> 
    handle_stop(From, Data);

connecting(Event, Msg, #{ log := Log, 
                          name := Name }=Data) ->
    Log({cmwsc, Name, connecting, ignored, Event, Msg}),
    {keep_state, Data}.

connected(From, stop, Data) -> 
    handle_stop(From, Data);

connected(info, {gun_down, _, _, closed, _, _}, Data) ->
    apply_reconnection_strategy(connected, Data); 

connected({call, _}, {out, Msg}, Data) ->
    Data2 = buffer(Msg, connected, Data),
    {keep_state, Data2};

connected({timeout, connecting}, _, #{ name := Name}=Data) ->
    cmkit:warning({cmwsc, Name, connected, timeout}),
    {next_state, disconnected, Data};

connected(Event, Msg, #{ log := Log, 
                         name := Name}=Data) ->
    Log({cmwsc, Name, connected, ignored, Event, Msg}),
    {next_state, connected, Data}.

upgrading(info, {gun_upgrade, _, _, _, _}, #{ log := Log,
                                              owner := Owner,
                                              name := Name }=Data) ->
    Log({cmwsc, Name, upgraded}),
    Data2 = flush(Data),
    Owner ! {ws, Name, up},
    {next_state, ready, Data2};

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

upgrading({call, _}, {out, Msg}, Data) ->
    Data2 = buffer(Msg, upgrading, Data),
    {keep_state, Data2};

upgrading({timeout, connecting}, _, #{ name := Name}=Data) ->
    cmkit:warning({cmwsc, Name, upgrading, timeout}),
    {next_state, disconnected, Data};

upgrading(Event, Msg, #{ log := Log, 
                         name := Name }=Data) ->
    Log({cmwsc, Name, upgrading, ignored, Event, Msg}),
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

    Log({cmwsc, Name, in, Msg}),

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

handle_stop({call, From}, _) ->
    {stop_and_reply, normal, ok(From)}.

terminate(Reason, _, #{ log := Log, name := Name, conn := Conn}) ->
    ok = gun:close(Conn),
    Log({cmwsc, Name, terminated, Reason}),
    ok.

maybe_monitor(Conn, #{ conn := Conn }=Data) -> Data;
maybe_monitor(Conn, #{ conn := OldConn,
                       monitor := OldRef }=Data) -> 
    erlang:demonitor(OldRef),
    NewRef = erlang:monitor(process, Conn),
    gun:close(OldConn),
    Data#{ conn => Conn,
           monitor => NewRef }.

connect(#{ config := #{ host := Host,
                        port := Port },
           monitor := OldRef,
           conn := OldConn } = Data) ->
    demonitor(OldRef),
    ok = gun:close(OldConn),

    {ok, Conn} = gun:open(cmkit:to_list(Host), Port, #{protocols => [http]}),
    MRef = monitor(process, Conn),
    Data#{ conn => Conn,
           monitor => MRef };

connect(#{ config := #{ host := Host,
                        port := Port}} = Data) ->

    {ok, Conn} = gun:open(cmkit:to_list(Host), Port, #{protocols => [http]}),
    MRef = monitor(process, Conn),
    Data#{ conn => Conn,
           monitor => MRef }.

ok(From) -> [{reply, From, ok}].

buffer(Msg, State, #{ name := Name,
                      buffer := Buffer }=Data) ->
    B2 = queue:in(Msg, Buffer),
    cmkit:warning({cmwsc, Name, State, buffered, Msg}),
    Data#{ buffer => B2 }.

flush(#{ buffer := Q }=Data) ->
    Q2 = flush(queue:out(Q), Data),
    Data#{ buffer => Q2 }.

flush({{value, Msg}, Q}, #{ name := Name} = Data) ->
    cmkit:warning({cmwsc, Name, flush, Msg}),
    ws_send(Msg, Data),
    flush(queue:out(Q), Data);

flush({empty, Q}, _) -> Q.

apply_reconnection_strategy(State, #{ log := Log, 
                                      owner := Owner,
                                      name := Name,
                                      config := #{ url := Url} = Config}=Data ) ->
    case maps:get(persistent, Config, true) of 
        true -> 
            Log({cmwsc, Name, Url, State, closed, reconnecting}),
            {next_state, connecting, Data};
        false ->
            Owner ! {ws, Name, down},
            Log({cmwsc, Name, Url, State, closed, terminating}),
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

apply_protocol(_, #{ log := Log,
                     name := Name}) -> 
    Log({cmwsc, Name, no_protocol}),
    ok.

try_decoders(#{ default := Decs }, Data) ->
    try_decoders(Decs, Data);

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
    end;

select_clause([Spec|_], _) ->
    {ok, Spec}.

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
    Log({cmwsc, Name, out, Msg}),
    gun:ws_send(Conn, {text, Raw}).


upgrade(Pid, Path, #{ headers := Headers }) ->
    gun:ws_upgrade(Pid, Path, to_headers_list(Headers));

upgrade(Pid, Path, _) ->
    gun:ws_upgrade(Pid, Path).


to_headers_list(Headers) ->
    maps:fold(fun(K, V, Acc) ->
                      [{cmkit:to_bin(K), cmkit:to_list(V)}|Acc]
              end, [], Headers).
