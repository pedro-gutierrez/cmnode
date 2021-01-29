-module(cmcron_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, load_crons(cmkit:to_atom(cmkit:env("CMCRON"))) }.

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


load_crons(false) ->
    cmkit:log({cmcron, disabled}),
    #{};

load_crons(true) ->
    load_crons().

load_crons() ->
    load_crons(cmconfig:crons(), #{}).

load_crons([], Data) -> Data;
load_crons([Cron|Rem], Data) ->
    load_crons(Rem, load_cron(Cron, Data)).

load_cron(#{ name := Name,
             schedule := Schedule,
             jobs := Jobs}, Data) -> 

    S = schedule(Schedule),
    Status = succeeded(attempted(Name, S, Jobs)),
    cmkit:log({cron, Name, length(Status)}),
    Data#{ Name => Status }.

succeeded(Results) ->
    lists:foldr(fun({ok, Ref}, Acc) ->
                        [Ref|Acc];
                   (_, Acc) ->
                        Acc
                end, [], Results).

attempted(Name, Schedule, Jobs) ->
    lists:map(fun(J) ->
                      case mfa(J) of 
                          {ok, MFA} ->
                              schedule_job({Schedule, MFA});        
                          Other ->
                              cmkit:error({cron, Name, job, J, mfa_error, Other}),
                              {error, Other}
                      end
              end, Jobs).

mfa(#{ module := M,
       function := F,
       args := Args }) ->

    {ok, {M, F, Args}};

mfa(#{ task := Task,
       settings := Settings }) ->

    case cmencode:encode(Task) of 
        {ok, T} ->
            case cmencode:encode(Settings) of 
                {ok, S} ->
                    {ok, {cmtask, schedule, [T, #{ settings => S}]}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

mfa(#{ task := Task }) ->

    case cmencode:encode(Task) of 
        {ok, T} ->
            {ok, {cmtask, schedule, [T, #{}]}};
        Other ->
            Other
    end.

schedule(#{ type := once,
            secs := Secs }) ->

    {once, Secs};

schedule(#{ type := every, 
            secs := Secs }) ->

    {daily, {every, {Secs, sec}, {between, {0, am}, {11, 59, pm}}}};

schedule(#{ type := daily, 
            hour := H,
            min := Min,
            period := P}) ->
    {daily, {H, Min, P}}.

status(Data) ->
    maps:fold(fun(K, V, Out) ->
                      Out#{ K => length(V) }
              end, #{}, Data).

schedule_job(Spec) ->
    Ref = erlcron:cron(Spec),
    {ok, Ref}.
