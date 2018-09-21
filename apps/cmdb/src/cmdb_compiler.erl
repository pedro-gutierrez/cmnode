-module(cmdb_compiler).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3
        ]).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init([]) ->
    {ok, Buckets} = cmdb_util:reload(),
    [ cmdb_writer_sup:start(B) || #{ storage := disc }=B <- Buckets ],
    {stop, normal}.

terminate(Reason, _, _) ->
    cmkit:log({cmdb, terminate, Reason}),
    ok.
