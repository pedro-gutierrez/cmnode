-module(cmdb_dynamo).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         offline/3,
         ready/3
        ]).
-record(data, {name}).

callback_mode() ->
    state_functions.

start_link(#{ name := Name }=Db) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Db], []).

init([#{name := Name}]) ->
    {ok, ready, #data{name=Name}}.

offline({call, From}, Msg, #data{name=Name}=Data) -> 
    cmkit:log({cmdb, Name, offline, Msg}),
    Res = {error, offline},
    {keep_state, Data, {reply, From, Res}}.

ready({call, From}, reset, Data) ->
    {keep_state, Data, {reply, From, {error, not_supported}}};

ready({call, From}, {get, K}, #data{name=Name}=Data) ->
    Res = case dets:lookup(Name, K) of 
        {error, R} -> {error, R};
        Objs -> Objs
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, K, V}, #data{name=Name}=Data) ->
    Res = dets:insert(Name, {K, V}),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, Pairs}, #data{name=Name}=Data) ->
    Res = dets:insert(Name, Pairs),
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{name=Name}) ->
    cmkit:log({cmdb_dynamo, node(), Name, terminated, Reason}),
    ok.
