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
    ok = cmsession:attach(Id, context, self()),
    Log = cmkit:log_fun(Debug),
    Log({ cmcore, App, Id, self() }),
    {ok, initializing, Session#{ spec => Spec, config => Config, log => Log }}.

initializing(cast, init,  #{app := App,
                                     config := Config,
                                     log := Log,
                                     spec := Spec, 
                                     id := Id}=Session) ->
    case cmcore_util:init(Spec, Config) of 
        {ok, Model, Cmds} -> 
            case cmsession:attach(Id, model, Model) of
                ok ->
                    Log({cmore, init, Model, Cmds}),
                    cmcore_util:cmds(Cmds, Model, Config, Session),
                    {next_state, ready, Session#{ model => Model }};
                {error, E} ->
                    Error = server_error(App, Session, init, E),
                    {stop, Error}
            end;
        {error, E} -> 
            Error = server_error(App, Session, init, E),
            {stop, Error}
    end.

ready(cast, {update, Data}, #{ app := App, 
                               id := Id, 
                               spec := Spec, 
                               config := Config,
                               model := Model, 
                               log := Log }=Session) ->
    
    Log({cmcore, data, App, Id, Data}),
    case cmcore_util:decode(Spec, Data, Config) of 
        {ok, Msg, Decoded} ->
            Log({cmcore, decoded, Msg, App, Id}),
            case cmcore_util:update_spec(Spec, Msg, Decoded, Model, Config) of 
                {ok, UpdateSpec} ->
                    Log({cmcore, updating, UpdateSpec, App, Id}),
                    case cmcore_util:update(Spec, UpdateSpec, Config, Decoded, {Model, []}) of
                        {ok, Model2, Cmds } ->
                            case cmsession:attach(Id, model, Model2) of
                                ok ->
                                    cmcore_util:cmds(Cmds, Model2, Config, Session),
                                    {keep_state, Session#{ model => Model2 }};
                                {error, E} ->
                                    server_error(App, Session, update, E),
                                    {keep_state, Session}
                            end;
                        {error, E} ->
                            server_error(App, Session, update, E),
                            {keep_state, Session}
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
                          log := Log }) ->
    
    Log({cmcore, terminate, App, Id, self()}),
    cmsession:delete(Id),
    {stop, normal}.

server_error(App, #{ id := Id}, Phase, Reason) ->
    Info = #{ status => error,
              app => App,
              phase => Phase,
              reason => Reason },
    cmkit:danger({cmcore, server_error, Info}),
    cmeffect:apply(notify, Info, Id),
    Info.

terminate(Reason, _, #{ app := App, id := Id}) ->
    cmkit:log({cmcore, App, Id, node(), terminated, Reason}),
    ok.

