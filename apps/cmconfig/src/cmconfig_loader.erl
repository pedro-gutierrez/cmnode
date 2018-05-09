-module(cmconfig_loader).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3,
         modified/1,
         load/0
        ]).
-record(data, {}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ready, #data{}}.

ready({call, From}, load, Data) ->
    Specs = specs(),
    compile_graph(Specs),
    compile_specs(sorted(Specs), []),
    {keep_state, Data, [{reply, From, ok}]};

ready({call, From}, {modified, Spec}, Data) ->
    compile_specs([Spec], [ancestors]),
    {keep_state, Data, [{reply, From, ok}]}.

terminate(Reason, _, _) ->
    cmkit:log({cmconfig_loader, terminate, Reason}),
    ok.

call(Msg) ->
    gen_statem:call({?MODULE, node()}, Msg).

modified(Spec) ->
    call({modified, Spec}).

load() ->
    call(load). 

specs() ->
    lists:map(fun({ok, Spec}) -> Spec end, cmyamls:all()).

sorted(Specs) ->
    lists:sort(fun cmconfig_util:compare/2, Specs).

compile_graph(Specs) ->
    lists:map(fun cmconfig_cache:build_graph/1, Specs).

compile_specs(Specs, Opts) ->
    lists:map(fun(Spec) ->
                 cmconfig_compiler:compile(Spec, Opts)     
              end, Specs).
