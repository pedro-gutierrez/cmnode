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

init([#{ config := Config,
         debug := Debug,
         spec := Spec }, Session]) ->
    {ok, initializing, Session#{ start => cmkit:micros(),
                                 debug => Debug,
                                 spec => Spec, 
                                 config => Config}}.

initializing({call, From}, init,  #{app := App,
                                    config := Config,
                                    debug := Debug,
                                    id := Id,
                                    spec := Spec}=Session) ->
    
    Log = cmkit:log_fun(Debug),
    ok = cmsession:attach(Id, context, self()),
    Effects = cmconfig:effects(),
    {ok, Effect} = cmcore_effect_sup:start_effect(Id),
    case cmcore_util:init(Spec, Config) of 
        {ok, Model, Cmds} -> 
            cmcore_util:cmds(Cmds, Model, Config, Session),
            {next_state, ready, Session#{ 
                                  log => Log,
                                  effect => Effect,
                                  effects => Effects,
                                  model => Model }, reply(From, ok)};
        {error, E} -> 
            server_error(App, Session, init, E),
            {stop_and_reply, normal, reply(From, error)}
    end.

ready(cast, {update, Data}, #{ app := App, 
                               spec := Spec, 
                               config := Config,
                               model := Model
                             }=Session) ->
    
    case cmcore_util:decode(Spec, Data, Config) of 
        {ok, Msg, Decoded} ->
            case cmcore_util:update_spec(Spec, Msg, Decoded, Model, Config) of 
                {ok, UpdateSpec} ->
                    case cmcore_util:update(Spec, UpdateSpec, Config, Decoded, {Model, []}) of
                        {ok, Model2, Cmds } ->
                            cmcore_util:cmds(Cmds, Model2, Config, Session),
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

ready(cast, terminate, #{ id := Id, 
                          effect := Effect }) ->
    
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

terminate(_, _, _)->
    ok.

reply(From, Msg) -> [{reply, From, Msg}].
