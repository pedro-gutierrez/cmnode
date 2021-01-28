-module(cmqueue_worker).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).

callback_mode() ->
    state_functions.

start_link(#{ worker := Name }=Spec) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Spec], []).

initial() ->
    #{ pending => 0,
       active => 0, 
       failed => 0 }.

init([#{ name := Name, 
         capacity := Cap }]) ->
    cmkit:log({queue, Name, ok}),
    Data = #{ name => Name,
              subscriptions => #{},
              cap => Cap,
              status => initial(),
              queue => queue:new(),
              running => #{} },
    {ok, ready, Data}.


ready(info, _Msg, Data) ->
    {keep_state, Data};


ready({call, From}, status, Data) ->
    Info = status_info(Data),
    {keep_state, Data, [{reply, From, {ok, Info}}]};

ready({call, From}, clear, #{ subscriptions := Subs }=Data) ->
    Data2 = clear(Data),
    Info = status_info(Data2),
    notify(Info, Subs),
    {keep_state, Data2, [{reply, From, {ok, Info}}]};

ready({call, From}, {subscribe, Topic, Id}, Data) ->
    Data2 = subscribe(Topic, Id, Data),
    Info = status_info(Data2),
    {keep_state,  Data2, [{reply, From, {ok, Info}}]};


ready(cast, {info, Id, JobInfo}, #{ name := Name,
                                    subscriptions := Subs,
                                    running := R }=Data) ->
    case maps:get(Id, R, undef) of 
        undef -> 
            cmkit:warning({cmqueue, Name, info, Id, unknown_job}),
            {keep_state, Data};
        Job ->
            Data2 = Data#{ running => R#{ Id => Job#{ info => JobInfo }}},
            Info = status_info(Data2),
            notify(Info, Subs),
            {keep_state, Data2}
    end;

ready({call, From}, {cancel, Id}, #{ name := Name,
                                     subscriptions := Subs,
                                     running := R }=Data) ->

    case maps:get(Id, R, undef) of 
        undef -> 
            Data2  =  remove_from_queue(Id, Data),
            Info = status_info(Data2),
            notify(Info, Subs),
            cmkit:log({cmqueue, Name, cancelled, Id}),
            {keep_state, Data2, [{reply, From, {ok, Info}}]};
        Job ->
            cancel_jobs([Job]),
            Data2 = active_job_finished(Id, Data),
            Data3 = start_next_job(Data2),
            Info = status_info(Data3),
            notify(Info, Subs),
            cmkit:log({cmqueue, Name, cancelled, Id}),
            {keep_state, Data3, [{reply, From, {ok, Info}}]}
    end;


ready(cast, {finished, Id}, #{ name := Name,
                               subscriptions := Subs,
                               status := #{ pending := 0 } }=Data) -> 

    cmkit:log({cmqueue, Name, finishing, Id}),
    Data2 = active_job_finished(Id, Data),
    Info = status_info(Data2),
    notify(Info, Subs),
    {keep_state, Data2};

ready(cast, {finished, Id}, #{ subscriptions := Subs,
                               status := #{ pending := _ } }=Data) -> 

    Data2 = active_job_finished(Id, Data),
    Data3 = start_next_job(Data2),
    Info = status_info(Data3),
    notify(Info, Subs),
    {keep_state, Data3};


ready({call, From}, {schedule, Job}, #{ subscriptions := Subs,
                                        status := #{ active := A, pending := P },
                                        cap := #{ concurrency := A, max := Max }}=Data) ->

    Data2 = case P < Max of 
                true -> 
                    enqueue(Job, Data);
                false -> 
                    Data
            end,

    Info = status_info(Data2),
    notify(Info, Subs),
    {keep_state, Data2, [{reply, From, {ok, Info}}]};


ready({call, From}, {schedule, Job}, #{  subscriptions := Subs,

                                         status := #{ active := A },
                                         cap := #{ concurrency := Max }}=Data) when A < Max ->
    Data2 = start_job(Job, Data),
    Info = status_info(Data2),
    notify(Info, Subs),
    {keep_state, Data2, [{reply, From, {ok, Info}}]}.

terminate(Reason, _, #{ name := Name}) ->
    cmkit:log({cmqueue, Name, terminate, Reason}),
    ok.

active_job_finished(Id, #{ status := #{ active := A } = Status,
                           running := R } = Data) ->

    Data#{ status => Status#{ active => A - 1},
           running => maps:without([Id], R) }.

status_info(#{ queue := Q, running := R, cap := #{ concurrency := Concurrency,
                                                   max := Max }}) ->
    Pending = lists:map(fun job_info/1, queue:to_list(Q)),
    Running = lists:map(fun job_info/1, maps:values(R)),

    #{ 
       active => #{ max => Concurrency,
                    current => length(Running),
                    items => Running },
       pending => #{ max => Max,
                     current => length(Pending),
                     items => Pending } }.

job_info(#{ id := Id,
            timestamp := Timestamp,
            info := Info  }) ->

    maps:merge(Info, #{ id => Id,
                        timestamp => Timestamp }).


clear(#{ status := Status}=Data) ->
    Data#{ subscriptions => #{},
           queue => queue:new(), 
           running => #{}, 
           status => Status#{ active => 0, pending => 0 }}.

subscribe(Topic, Id, #{ subscriptions := Subs }=Data) ->
    Data#{ subscriptions => Subs#{ Id => Topic }}.


notify(Info, Subs) ->
    maps:fold( fun(Id, Topic, Sent) ->
                       cmcore:notify(Id, #{ Topic => Info }),
                       Sent + 1
               end, 0, Subs),
    ok.


cancel_jobs(Jobs) -> 
    [ cancel_job(J) || J <- Jobs].

cancel_job(#{ spec := #{ stop :=  {M, F, Args} }}) ->
    apply(M, F, Args);

cancel_job(_) -> ok.

start_job(#{ id := Id,
             timestamp := _,
             info:= _,
             spec := #{ start := {M, F, Args} }} = Job, #{ name := Name,
                                                           status := #{ active := A } = Status,
                                                           running := R, name := Name }=Data) ->

    case apply(M, F, Args) of 
        ok -> 
            cmkit:log({cmqueue, Name, started, Job}),
            Data#{ status => Status#{ active => A + 1}, 
                   running => R#{ Id => Job } };
        {error, E} ->
            cmkit:danger({cmqueue, Name, failed, Job, E}),
            start_next_job(Data)
    end.

enqueue(Job, #{ queue := Q, 
                status := #{ pending := P}=Status }=Data) ->
    Q2 = queue:in(Job, Q),
    Data#{ queue => Q2, 
           status => Status#{ pending => P + 1}}.

dequeue(#{ queue := Q, 
           status := #{ pending := P}=Status }=Data) ->
    {{value, Job}, Q2}  = queue:out(Q),
    {Job, Data#{ queue => Q2, 
                 status => Status#{ pending => P - 1}}}.

remove_from_queue(_, #{ name := Name, 
                        status := #{ pending := 0 }}=Data) ->
    cmkit:warning({cmqueue, Name, cancel, empty_queue}),
    Data;

remove_from_queue(Id, #{ name := Name, 
                         queue := Q, 
                         status := #{ pending := P } = Status }=Data) ->
    {L2, Removed} = lists:partition(fun(#{ id := JobId }) ->
                                            Id =/= JobId
                                    end, queue:to_list(Q)),
    case Removed of 
        [#{ id := Id }] ->
            Q2 = queue:from_list(L2),
            Data#{ queue => Q2, status => Status#{ pending => P -1 } };
        _ -> 
            cmkit:warning({cmqueue, Name, cancel, Id, unexpected, Removed}),
            Data
    end.


start_next_job(#{ status := #{ pending := 0 }}=Data) ->
    Data;

start_next_job(#{ status := #{ pending := _ }}=Data) ->

    {NextJob, Data2} = dequeue(Data),
    start_job(NextJob, Data2).
