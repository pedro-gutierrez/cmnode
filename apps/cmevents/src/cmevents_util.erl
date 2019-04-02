-module(cmevents_util).
-export([reload/0]).
-export([writer/1,reader/1,store_id/1]).
-export([write/2]).

%% Reload the yaml config and look for buckets
%% configured to behave as event stores
reload() ->
    Buckets = [ #{ name => cmkit:to_atom(Name),
                   debug => cmkit:to_atom(maps:get(<<"debug">>, Spec, <<"false">>))
                 } || {ok, #{ <<"name">> := Name,
                              <<"spec">> := #{ 
                                  <<"role">> := <<"es">> } = Spec}} <- cmkit:yamls(bucket)] ,
    {ok, Buckets}.

%% Returns the topic to which event store 
%% replicas should be subscribed, for a given bucket name
writer(N) -> {event_store, writer, N}.

%% Get the pid of the local writer the given
%% bucket
local_writer(N) ->
    T = writer(N),
    case cmbus:local(T) of
        [] ->
            {error, #{ topic => T,
                       node => node(),
                       error => no_local_pid }};
        [Pid] ->
            {ok, Pid}
    end.

%% Returns the topic to which event store
%% readers should be subscribed, for a given bucket name
reader(N) -> {event_store, reader, N}.

%% Persist an event to the given
%% bucket
write(Bucket, Msg) ->
    case local_writer(Bucket) of 
        {ok, Pid} ->
            gen_server:call(Pid, Msg);
        Other ->
            Other
    end.

store_id(#{ name := Name }) ->
    cmkit:to_atom(<<(cmkit:to_bin(Name))/binary, "_events">>).
