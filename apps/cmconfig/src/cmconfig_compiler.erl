-module(cmconfig_compiler).
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
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    cmconfig_util:reload(),
    {stop, normal}.

terminate(Reason, _, _) ->
    cmkit:log({cmconfig, compiler, terminate, Reason}),
    ok.
