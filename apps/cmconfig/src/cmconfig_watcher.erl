-module(cmconfig_watcher).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).
-record(data, {dir, watcher}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Dir = cmkit:etc(),
    {ok, Pid} = cmkit:watch(Dir),
    cmkit:log({cmconfig, watching, Pid}),
    {ok, ready, #data{dir=Dir, watcher=Pid}}.

ready(info, {_, {fs, file_event}, {File, Events}}, Data) ->
    Ext = filename:extension(File),
    [handle_file_event(File, Ext, Ev) || Ev <- Events],
    {next_state, ready, Data}.

terminate(Reason, _, _) ->
    cmkit:log({cmdb, terminate, Reason}),
    ok.

handle_file_event(File, ".yml", modified) ->
    spawn( fun() ->
                   case cmkit:yaml(File) of 
                       {ok, #{ <<"type">> := Type, 
                               <<"name">> := Name, 
                               <<"version">> := Version }} ->
                           cmkit:log({cmconfig, modified, Type, Name, Version}),
                           cmconfig_util:reload(Type);
                       _ ->
                           cmkit:danger({cmconfig, error, File})
                   end
           end),
    ok;

handle_file_event(_, _, _) -> ok.
