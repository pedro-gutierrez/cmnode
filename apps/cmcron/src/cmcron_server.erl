-module(cmcron_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(reload, _, Data) ->
    cancel_existing_jobs(Data),
    Data2 = load_crons(),
    {reply, {ok, status(Data2)}, Data2};

handle_call(status, _, Data) ->
    {reply, {ok, status(Data)}, Data}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info(_, Data) ->
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _) ->
    cmkit:log({cmcron, server, terminated, Reason}),
    ok.


cancel_existing_jobs(Data) ->
    lists:foreach(fun(Refs) ->
                       lists:foreach(fun(Ref) ->
                            erlcron:cancel(Ref)
                                      end, Refs)
                   end, maps:values(Data)).


load_crons() ->
    load_crons(cmconfig:crons(), #{}).

load_crons([], Data) -> Data;
load_crons([Cron|Rem], Data) ->
    load_crons(Rem, load_cron(Cron, Data)).

load_cron(#{ name := Name,
             schedule := Schedule,
             jobs := Jobs}, Data) -> 
    Data#{ Name => 
           [ erlcron:cron({ map_schedule(Schedule), {M, F, Args}})
             || #{ module := M,
                   function := F,
                    args := Args } <- Jobs ]}.

map_schedule(#{ type := daily, 
                hour := H,
                min := Min,
                period := P}) ->
    {daily, {H, Min, P}}.

status(Data) ->
    maps:fold(fun(K, V, Out) ->
                Out#{ K => length(V) }
              end, #{}, Data).







