-module(cmcore).
-export([init/3, update/2, update/5, notify/2, terminate/1]).


init(Pid, #{ name := App, 
             spec := Spec,
             config := Config0 }, Log) ->
    Log({App, Pid, init}),
    Config = case maps:get(encoders, Spec, undef) of
                 undef -> Config0;
                 Encs -> Config0#{ encoders => Encs }
             end,

    case cmcore_util:init(Spec, Config) of
        {ok, Model, Cmds} ->
            cmcore_util:cmds(Cmds, Model, Config, Pid, Log),
            {ok, Model, Config};
        {error, E} ->
            err(App, Pid, init, E)
    end.

update(Pid, #{ name := App,
               spec := Spec,
               config := Config } = AppSpec, Data, Model, Log) ->
    Log({App, Pid, in, Data}),
    case cmcore_util:decode(Spec, Data, Config) of
        {ok, Msg, Decoded} ->
            Log({App, Pid, decoded, Msg}),
            update(Pid, AppSpec,  Msg, Decoded, Model, Log);
        {error, no_match} ->
            cmkit:warning({App, Pid, no_match, Data}),
            update(Pid, AppSpec,  no_match, Data, Model, Log);
        {error, E} ->
            err(App, Pid, update, #{ data => Data, reason => E })
    end.

update(Pid, #{ name := App,
               spec := Spec,
               config := Config }, Msg, Data, Model, Log) ->
    
    case cmcore_util:update_spec(Spec, Msg, Data, Model, Config) of
        {ok, UpdateSpec} ->
            case cmcore_util:update(Spec, UpdateSpec, Config, Data, {Model, []}) of
                {ok, Model2, Cmds } ->
                    cmcore_util:cmds(Cmds, Model2, Config, Pid, Log),
                    {ok, Model2};
                {error, E} ->
                    err(App, Pid, update, #{ data => Data, reason => E})
            end;
        {error, E} ->
            err(App, Pid, update, #{ data => Data, reason => E})
    end.

update(Pid, Data) when is_pid(Pid) ->
    Pid ! {update, Data}.

notify(Pid, Data) when is_pid(Pid) ->
    Pid ! Data.

terminate(Pid) when is_pid(Pid) ->
    Pid ! terminate.


err(App, Pid, Phase, Info) ->
    E = #{ status => error,
           app => App,
           pid => Pid,
           phase => Phase,
           info => Info },
    cmkit:danger({cmcore, E}),
    {error, E}.

