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
        {ok, #{ modules := Modules }} ->
            case cmcore_util:init(Modules) of 
                {ok, Model, Cmds} -> 
                    case cmsession:attach(Id, model, Model) of
                        ok ->
                            cmcore_util:cmds(Cmds, Model, Session),
                            {next_state, ready, Session#{ model => Model, 
                                                          modules => Modules }};
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


ready(cast, {update, Data}, #{app := App, id := Id, model := Model, modules := Modules}=Session) ->
    cmkit:log({cmcore, update, App, Id, Data}),
    case cmcore_util:decode(Modules, Data) of 
        {ok, Module, Msg, Decoded} ->
            case cmcore_util:update_spec(Modules, Module, Msg) of 
                {ok, Spec} ->
                    case cmcore_util:update(Modules, Spec, Decoded, {Model, []}) of
                        {ok, Model2, Cmds } ->
                            case cmsession:attach(Id, model, Model2) of
                                ok ->
                                    cmcore_util:cmds(Cmds, Model2, Session),
                                    {keep_state, Session#{ model => Model}};
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
            server_error(App, Session, update, E),
            {keep_state, Session}
    end.

server_error(App, Session, Phase, Reason) ->
    Data = #{ status => error,
              data => #{
                app => App,
                reason => Reason,
                phase => Phase
               }
            },
    cmkit:log({cmcore, Data}),
    cmeffect:apply(notify, Data, Session),
    Data.


terminate(Reason, _, #{ app := App, id := Id}) ->
    cmkit:log({cmcore, App, Id, node(), terminated, Reason}),
    ok.

