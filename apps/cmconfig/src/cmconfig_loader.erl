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
    case cmconfig_util:parse() of 
        {ok, Specs} ->
            compile_graph(Specs),
            compile_specs(sort(Specs), []),
            {keep_state, Data, [{reply, From, ok}]};
        {error, Errors} ->
            cmkit:danger({cmconfig, loader, errors, Errors}),
            {stop, compile_errors}
    end;

ready({call, From}, {modified, Spec}, Data) ->
    compile_specs([Spec], [ancestors]),
    {keep_state, Data, [{reply, From, ok}]}.

terminate(Reason, _, _) ->
    cmkit:danger({cmconfig_loader, terminate, Reason}),
    ok.

call(Msg) ->
    gen_statem:call({?MODULE, node()}, Msg).

modified(Spec) ->
    call({modified, cmconfig_util:ranked(Spec)}).

load() ->
    call(load). 


compile_graph(Specs) ->
    lists:map(fun cmconfig_cache:build_graph/1, Specs).


sort(Specs) -> 
    Ranked = lists:map(fun cmconfig_util:ranked/1, Specs),
    lists:sort(fun(#{ <<"rank">> := R1}, #{ <<"rank">> := R2 }) ->
                R1 =< R2
               end, Ranked).


compile_specs(Specs, Opts) ->
    lists:map(fun(Spec) ->
                 cmconfig_compiler:compile(Spec, Opts)     
              end, Specs).
