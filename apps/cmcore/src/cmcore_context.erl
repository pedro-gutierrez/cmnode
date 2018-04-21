-module(cmcore_context).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         initializing/3,
         ready/3
        ]).

callback_mode() ->
    state_functions.

start_link(Session) ->
    gen_statem:start_link(?MODULE, [Session], []).

init([#{ id := Id, app := App }=Session]) ->
    ok = cmsession:attach(Id, context, self()),
    cmkit:log({cmcore, init, App, Id}),
    {ok, initializing, Session}.

initializing(cast, {init, _Data},  #{app := App, id := Id}=Session) ->
    cmkit:log({cmcore, init, App, Id}),
    case cmconfig:app(App) of 
        {ok, #{ spec := Spec }} ->
            case cmcore_util:init(Spec) of 
                {ok, Model, Cmds} -> 
                    case cmsession:attach(Id, model, Model) of
                        ok ->
                            cmcore_util:cmds(Cmds, Model, Session),
                            {next_state, ready, Session#{ model => Model, 
                                                          spec => Spec }};
                        {error, E} ->
                            Error = server_error(App, Session, init, E),
                            {stop, Error}
                    end;
                {error, E} -> 
                    Error = server_error(App, Session, init, E),
                    {stop, Error}
            end;
        {error, E} ->
            Error = server_error(App, Session, init, E),
            {stop, Error}
    end.


ready(cast, {update, Data}, #{ app := App, id := Id, spec := Spec, model := Model }=Session) ->
    cmkit:log({cmcore, update, App, Id, Data}),
    case cmcore_util:decode(Spec, Data) of 
        {ok, Msg, Decoded} ->
            cmkit:log({cmcore, decoded, Msg, Decoded}),
            case cmcore_util:update_spec(Spec, Msg, Model) of 
                {ok, UpdateSpec} ->
                    cmkit:log({cmcore, spec, UpdateSpec}),
                    case cmcore_util:update(Spec, UpdateSpec, Decoded, {Model, []}) of
                        {ok, Model2, Cmds } ->
                            case cmsession:attach(Id, model, Model2) of
                                ok ->
                                    cmcore_util:cmds(Cmds, Model2, Session),
                                    {keep_state, Session#{ model => Model2 }};
                                {error, E} ->
                                    Error = server_error(App, Session, update, E),
                                    {stop, Error}
                            end;
                        {error, E} ->
                            Error = server_error(App, Session, update, E),
                            {stop, Error}
                    end;
                {error, E} ->
                    Error = server_error(App, Session, update, E),
                    {stop, Error}
            end;
        {error, E} -> 
            Error = server_error(App, Session, update, E),
            {stop, Error}

    end.

server_error(App, #{ id := Id} = Session, Phase, Reason) ->
    Data = #{ status => error,
              data => #{
                app => App,
                reason => Reason,
                phase => Phase
               }
            },
    cmkit:log({cmcore, server_error, App, Id, Reason}),
    cmeffect:apply(notify, Data, Session),
    Data.

terminate(Reason, _, #{ app := App, id := Id}) ->
    cmkit:log({cmcore, App, Id, node(), terminated, Reason}),
    ok.

