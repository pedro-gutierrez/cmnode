-module(cmcore_context).
-behaviour(gen_statem).
-export([
         start_link/2,
         init/1, 
         callback_mode/0, 
         terminate/3,
         initializing/3,
         ready/3
        ]).

callback_mode() ->
    state_functions.

start_link(Spec, Session) ->
    gen_statem:start_link(?MODULE, [Spec, Session], []).

init([#{ debug := Debug, 
         config := Config,
         spec := Spec }, #{ id := Id, app := App }=Session]) ->
    Start = cmkit:micros(),
    Log = cmkit:log_fun(Debug),
    Effects = cmconfig:effects(),
    {ok, Effect} = cmcore_effect_sup:start_effect(Id),
    ok = cmsession:attach(Id, context, self()),
    InitTime = cmkit:elapsed(Start),
    Log({ cmcore, App, Id, self(), InitTime }),
    {ok, initializing, Session#{ start => Start,
                                 effect => Effect, 
                                 effects => Effects,
                                 spec => Spec, 
                                 config => Config, 
                                 log => Log }}.

initializing(cast, init,  #{app := App,
                            config := Config,
                            log := Log,
                            spec := Spec}=Session) ->
    Start = cmkit:micros(),
    case cmcore_util:init(Spec, Config) of 
        {ok, Model, Cmds} -> 
            cmcore_util:cmds(Cmds, Model, Config, Session),
            Log({cmore, init, Model, Cmds, cmkit:elapsed(Start)}),
            {next_state, ready, Session#{ model => Model }};
        {error, E} -> 
            server_error(App, Session, init, E),
            {stop, normal}
    end.

ready(cast, {update, Data}, #{ app := App, 
                               id := Id, 
                               spec := Spec, 
                               config := Config,
                               model := Model, 
                               log := Log }=Session) ->
    
    Start = cmkit:micros(),
    Log({cmcore, data, App, Id, Data}),
    case cmcore_util:decode(Spec, Data, Config) of 
        {ok, Msg, Decoded} ->
            E1 = cmkit:elapsed(Start),
            Log({cmcore, decoded, Msg, App, Id}),
            case cmcore_util:update_spec(Spec, Msg, Decoded, Model, Config) of 
                {ok, UpdateSpec} ->
                    E2 = cmkit:elapsed(Start),
                    case cmcore_util:update(Spec, UpdateSpec, Config, Decoded, {Model, []}) of
                        {ok, Model2, Cmds } ->
                            E3 = cmkit:elapsed(Start),
                            cmcore_util:cmds(Cmds, Model2, Config, Session),
                            E4 = cmkit:elapsed(Start),
                            Log({cmcore, updated, Msg, App, Id, #{ decode => E1,
                                                                   select => E2-E1,
                                                                   update => E3-E2,
                                                                   cmds => E4-E3 }}), 
                            {keep_state, Session#{ model => Model2 }};
                        {error, E} ->
                            server_error(App, Session, update, E),
                            {stop, normal}
                    end;
                {error, E} ->
                    server_error(App, Session, update, E),
                    {keep_state, Session}
            end;
        {error, E} -> 
            server_error(App, Session, update, #{ data => Data, reason => E}),
            {keep_state, Session}

    end;

ready(cast, terminate, #{ app := App,
                          id := Id, 
                          log := Log,
                          effect := Effect }) ->
    
    Log({cmcore, terminating, App, Id, self()}),
    cmsession:delete(Id),
    ok = cmcore_effect:stop(Effect),
    {stop, normal}.

server_error(App, Session, Phase, Reason) ->
    Info = #{ status => error,
              app => App,
              phase => Phase,
              reason => Reason },
    cmkit:danger({cmcore, server_error, Info}),
    cmcore_util:apply_effect(notify, Info, Session),
    gen_statem:cast(self(), terminate),
    Info.

terminate(Reason, _, #{ app := App, id := Id, start := Start}) ->
    Elapsed = cmkit:elapsed(Start), 
    cmkit:log({cmcore, App, Id, node(), terminated, Reason, Elapsed}),
    ok.

