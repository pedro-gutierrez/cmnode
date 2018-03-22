-module(cmweb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    [ start(App) || App <- cmconfig:apps() ],
    cmweb_sup:start_link().

stop(_) ->
    ok.

start(#{ name := Name, 
         acceptors := Acceptors,
         port := Port }) ->

    Dispatch = cowboy_router:compile([{'_', routes(Name)}]),
    {ok, _} = cowboy:start_clear(Name, 
                                 [{port, Port}, {num_acceptors, Acceptors}],
                                 #{env => #{dispatch => Dispatch},
                                  stream_handlers => [cowboy_compress_h,
                                                     cowboy_stream_h]}),
    cmkit:log({cmweb, Name, Port, ok}).

routes(Name) ->
    
    State = #{ name => Name },
    AppDir = atom_to_list(Name),

    [
        {"/api/[...]", cmweb_handler, State},
        {"/files/[...]", cmweb_handler, State},
        {"/ws", cmweb_handler, State},
        {"/", cowboy_static, {priv_file, cmweb, AppDir ++ "/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, cmweb, AppDir}}
    ].
