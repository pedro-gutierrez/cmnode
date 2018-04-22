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
         spec := Spec }, #{ id := Id, app := App }=Session]) ->
    ok = cmsession:attach(Id, context, self()),
    Log = cmkit:log_fun(Debug),
    Log({ cmcore_context, App, Id, self() }),
    {ok, initializing, Session#{ spec => Spec, log => Log }}.

initializing(cast, {init, _Data},  #{app := App, 
                                     spec := Spec, 
                                     id := Id}=Session) ->
    case cmcore_util:init(Spec) of 
        {ok, Model, Cmds} -> 
            case cmsession:attach(Id, model, Model) of
                ok ->
                    cmcore_util:cmds(Cmds, Model, Session),
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
                               model := Model, 
                               log := Log }=Session) ->
    Log({cmcore_context, data, Data, App, Id}),
    case cmcore_util:decode(Spec, Data) of 
        {ok, Msg, Decoded} ->
            Log({cmcore_context, decoded, Msg, App, Id}),
            case cmcore_util:update_spec(Spec, Msg, Model) of 
                {ok, UpdateSpec} ->
                    Log({cmcore_context, update, UpdateSpec, App, Id}),
                    case cmcore_util:update(Spec, UpdateSpec, Decoded, {Model, []}) of
                        {ok, Model2, Cmds } ->
                            Log({cmcore_context, model_cmd, Model2, Cmds, App, Id}),
                            case cmsession:attach(Id, model, Model2) of
                                ok ->
                                    cmcore_util:cmds(Cmds, Model2, Session),
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

    end.

server_error(App, Session, Phase, Reason) ->
    Info = #{ status => error,
              app => App,
              phase => Phase,
              reason => Reason },
    cmkit:log(Info),
    cmeffect:apply(notify, Info, Session),
    Info.

terminate(Reason, _, #{ app := App, id := Id}) ->
    cmkit:log({cmcore, App, Id, node(), terminated, Reason}),
    ok.

