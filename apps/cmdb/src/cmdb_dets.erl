-module(cmdb_dets).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         offline/3,
         ready/3
        ]).
-define(BACKUPS_BUCKET, "in-fullpass-backups").
-record(data, {name, dir, file, db}).


callback_mode() ->
    state_functions.

start_link(#{ name := Name }=Db) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Db], []).

init([#{name := Name, hosts := Hosts}=Db]) ->
    {Dir, Filename } = data_file(Name),
    cmkit:mkdirp(Dir),
    case dets:open_file(Name, [{access, read_write},
                               {type, bag},
                               {file, Filename}
                              ]) of 
        {ok, Name} -> 
            case cmcloud:is_local(Hosts) of 
                true -> 
                    cmkit:log({cmdb, Name, dets, started}),
                    {ok, ready, #data{name=Name, db=Db, dir=Dir, file=Filename}};
                false ->
                    cmkit:log({cmdb, Name, dets, offline, Hosts}),
                    {ok, offline, #data{name=Name, db=Db, dir=Dir, file=Filename}}
            end;
        {error, E} ->
            cmkit:log({cmdb_dets, Name, dets, error, E}),
            {error, E}
    end.

offline({call, From}, Msg, #data{name=Name}=Data) -> 
    cmkit:log({cmdb, Name, offline, Msg}),
    Res = {error, offline},
    {keep_state, Data, {reply, From, Res}}.

ready({call, From}, reset, #data{name=Name}=Data) ->
    Res = case dets:delete_all_objects(Name) of 
        {error, R} -> {error, R};
        true -> ok;
        ok -> ok
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {find, Type}, #data{name=Name}=Data) ->
    Res = case dets:match(Name, {{Type, '_'}, '$1'}) of 
        {error, R} -> {error, R};
        Objs when is_list(Objs) -> {ok, lists:flatten(Objs)}
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {get, K}, #data{name=Name}=Data) ->
    Res = case dets:lookup(Name, K) of 
        {error, R} -> {error, R};
        [] -> not_found;
        Objs -> {ok, lists:map(fun({_, V}) -> V end, Objs)}
    end,
    {keep_state, Data, {reply, From, Res}};
    
ready({call, From}, {put, K, V}, #data{name=Name}=Data) ->
    Res = dets:insert(Name, {K, V}),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, Pairs}, #data{name=Name}=Data) ->
    Res = dets:insert(Name, Pairs),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put_new, Pairs}, #data{name=Name}=Data) ->
    Res = case dets:insert_new(Name, Pairs) of 
              true -> ok;
              Other -> error
          end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, backup, #data{dir=D, name=Name}=Data) ->
    ZipFile = string:join([ D, "data.zip" ], "/"),
    Res = case  zip:create(ZipFile, ["data.db"], [{cwd, D}]) of 
        {ok, ZipFile} -> 
            case file:read_file(ZipFile) of 
                {ok, Bytes} -> 
                    Timestamp = cmkit:fmt_date(),
                    BackupName = backupName(Timestamp, Name),
                    case cms3:put_file(?BACKUPS_BUCKET, BackupName, Bytes) of 
                        ok -> {ok, Timestamp};
                        Other -> Other
                    end;
                {error, E} -> 
                    {error, E}
            end;
        {error, E} -> 
            {error, E}
    end,
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {restore, Timestamp},  #data{dir=D, name=Name}=Data) ->
    RemoteName = backupName(Timestamp, Name),
    Res = case cms3:get_file(?BACKUPS_BUCKET, RemoteName) of 
              {ok, Bytes} -> 
                case zip:extract(Bytes, [{cwd, D}]) of 
                    {ok, _} -> ok;
                    Other -> Other
                end
          end,
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{name=Name}) ->
    cmkit:log({cmdb_dets, node(), terminated, Reason}),
    dets:close(Name),
    ok.

backupName(Timestamp, Name) ->
    Timestamp
    ++ "-"
    ++ atom_to_list(Name)
    ++ ".zip".

data_file(Name) -> 
    DbDir = string:join([cmkit:home(), "data", atom_to_list(Name)], "/"),
    DbFile = string:join([DbDir, "data.db" ], "/"),
    { DbDir, DbFile }. 
