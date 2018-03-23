-module(cmdb_ets).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         offline/3,
         ready/3
        ]).
-record(data, {name, tid, db}).


callback_mode() ->
    state_functions.

start_link(#{ name := Name }=Db) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Db], []).

init([#{name := Name, hosts := Hosts}=Db]) ->
    case ets:new(Name, [private, bag]) of 
        {error, E} ->
            cmkit:log({cmdb_dets, Name, dets, error, E}),
            {error, E};
        Tid -> 
            case cmcloud:is_local(Hosts) of 
                true -> 
                    cmkit:log({cmdb, Name, ets, started}),
                    {ok, ready, #data{name=Name, db=Db, tid=Tid}};
                false ->
                    cmkit:log({cmdb, Name, ets, offline, Hosts}),
                    {ok, ready, #data{name=Name, db=Db, tid=Tid}}
            end
    end.

offline({call, From}, Msg, #data{name=Name}=Data) -> 
    cmkit:log({cmdb, Name, offline, Msg}),
    Res = {error, offline},
    {keep_state, Data, {reply, From, Res}}.

ready({call, From}, {find, Type}, #data{tid=Tid}=Data) ->
    Res = case ets:match(Tid, {{Type, '_'}, '$1'}) of 
        {error, R} -> {error, R};
        Objs when is_list(Objs) -> {ok, lists:flatten(Objs)}
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {get, K}, #data{tid=Tid}=Data) ->
    Res = case ets:lookup(Tid, K) of 
        {error, R} -> {error, R};
        [] -> not_found;
        Objs -> {ok, lists:map(fun({_, V}) -> V end, Objs)}
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, Pairs}, #data{tid=Tid}=Data) ->
    Res = case ets:insert(Tid, Pairs) of 
        true -> ok;
        _ -> error
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put_new, Pairs}, #data{tid=Tid}=Data) ->
    Res = case ets:insert_new(Tid, Pairs) of 
        true -> ok;
        _ -> error
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, backup, Data) ->
    Res = {error, not_supported},
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {restore, _},  Data) ->
    Res = {error, not_supported},
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{name=Name}) ->
    cmkit:log({cmdb, Name, node(), terminated, Reason}),
    ok.
