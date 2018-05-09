-module(cmconfig_compiler).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3,
         compile/2
        ]).
-record(data, {}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    cmkit:log({cmconfig_compiler, ready}),
    {ok, ready, #data{}}.

ready({call, From}, {compile, Spec, Opts}, Data) ->
    Res = compile_internal(Spec, Opts), 
    {next_state, ready, Data, [{reply, From, Res}]};

ready({call, _}, Msg, Data) ->
    cmkit:log({cmconfig_compiler, ignored, Msg}),
    {next_state, ready, Data}.

terminate(Reason, _, _) ->
    cmkit:log({cmconfig, compiler, terminated, Reason}),
    ok.

call(Msg) ->
    gen_statem:call({?MODULE, node()}, Msg).

compile(Spec, Opts) ->
    call({compile, Spec, Opts}).


compile_internal(#{
               <<"name">> := N,
               <<"type">> := T,
               <<"version">> := Version }=Spec, Opts) ->
    Type = cmkit:to_atom(T),
    Name = cmkit:to_atom(N),
    Res = case compile_related(Type, Name, Opts, children, fun cmconfig_cache:children/2) of 
        ok ->
            case cmconfig_util:compile(Spec) of 
                {ok, Compiled} -> 
                    case cmconfig_cache:update(Compiled) of 
                        ok -> 
                            compile_related(Type, Name, Opts, ancestors, fun cmconfig_cache:ancestors/2);
                        Other -> Other
                    end;
                Other -> Other
            end;
        Other -> Other
    end,

    cmkit:log({cmconfig, compile, Res, Type, Name, Version, Opts}).


compile_related(Type, Name, Opts, Flag, GeneratorFun) ->
    case lists:member(Flag, Opts) of
        false -> ok;
        true -> 
            case GeneratorFun(Type, Name) of 
                {ok, Related} ->
                    cmkit:log({cmconfig, compiling_related, Type, Name, Flag, Opts}),
                    lists:map( fun({T, N}) ->
                                       ok = resolve_and_compile(T, N, Opts)
                               end, Related),
                    ok;
                _ ->
                    cmkit:log({cmconfig, no_relations, Type, Name, Flag, Opts}),
                    ok
            end
    end.

resolve_and_compile(T, N, Opts) ->
    case cmyamls:of_type_name(T, N) of 
        [{ok, Spec}] -> compile_internal(Spec, Opts);
        Other -> {error, {yaml_error, T, N, Other}}
    end.
