-module(cmqueue_worker).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         green/3,
         yellow/3,
         red/3
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
              queue => queue:new() },
    {ok, state(Data), Data}.

green(info, _Msg, Data) ->
    {next_state, state(Data), Data};

green(cast, {failed, Job, Reason}, Data) -> 
    
    #{ status := #{ active := A }=Status} = Data2 = failed(Job, Reason, Data),
    Data3 = Data2#{ status => Status#{ active => A - 1 }},
    {next_state, state(Data3), Data3};

green(cast, finished, #{ subscriptions := Subs,
                         status := #{ active := A}=Status}=Data) -> 
    Data2 = Data#{ status => Status#{ active => A - 1}},
    Info = status_info(Data2),
    notify(Info, Subs),
    {next_state, state(Data2), Data2};

green({call, From}, status, Data) ->
    Info = status_info(Data),
    State = state(Data),
    {next_state, State, Data, [{reply, From, {ok, Info}}]};

green({call, From}, clear, #{ subscriptions := Subs }=Data) ->
    Data2 = clear(Data),
    Info = status_info(Data2),
    State = state(Data2),
    notify(Info, Subs),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

green({call, From}, {subscribe, Topic, Id}, Data) ->
    Data2 = subscribe(Topic, Id, Data),
    Info = status_info(Data2),
    State = state(Data2),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

green({call, From}, {schedule, Job}, #{ subscriptions := Subs,
                                        status := #{ active := A },
                                        cap := #{ concurrency := A }}=Data) ->
    
    Data2 = enqueue(Job, Data),
    Info = status_info(Data2),
    State = state(Data2),
    notify(Info, Subs),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

green({call, From}, {schedule, Job}, #{  subscriptions := Subs,
                                         status := #{ active := A }=Status}=Data) ->
    Data2 = Data#{ status => Status#{ active => A + 1}},
    Data3 = start(Job, Data2),
    Info = status_info(Data3),
    State = state(Data3),
    notify(Info, Subs),
    {next_state, State, Data3, [{reply, From, {ok, Info}}]}.

yellow(info, _Msg, Data) ->
    {next_state, state(Data), Data};

yellow(cast, {failed, Job, Reason}, #{ subscriptions := Subs }=Data) ->
    Data2 = failed(Job, Reason, Data),
    {Job, Data3} = dequeue(Data2),
    Data4 = start(Job, Data3),
    Info = status_info(Data4),
    notify(Info, Subs),
    {next_state, state(Data4), Data4};

yellow(cast, finished, #{ subscriptions := Subs }=Data) ->
    {Job, Data2} = dequeue(Data),
    Data3 = start(Job, Data2),
    Info = status_info(Data3),
    State = state(Data3),
    notify(Info, Subs),
    {next_state, State, Data3};

yellow({call, From}, status, Data) ->
    Info = status_info(Data),
    State = state(Data),
    {next_state, State, Data, [{reply, From, {ok, Info}}]};

yellow({call, From}, clear, #{ subscriptions := Subs }=Data) ->
    Data2 = clear(Data),
    Info = status_info(Data2),
    State = state(Data2),
    notify(Info, Subs),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

yellow({call, From}, {subscribe, Topic, Id}, Data) ->
    Data2 = subscribe(Topic, Id, Data),
    Info = status_info(Data2),
    State = state(Data2),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

yellow({call, From}, {schedule, Job}, #{ subscriptions := Subs }=Data) ->
    Data2 = enqueue(Job, Data),
    Info = status_info(Data2),
    State = state(Data2),
    notify(Info, Subs),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]}.

red(info, _Msg, Data) ->
    {next_state, state(Data), Data};

red(cast, {failed, Job, Reason}, #{ subscriptions := Subs }=Data) ->
    Data2 = failed(Job, Reason, Data),
    {Job, Data3} = dequeue(Data2),
    Data4 = start(Job, Data3),
    Info = status_info(Data4),
    State = state(Data4),
    notify(Info, Subs),
    {next_state, State, Data4};

red(cast, finished, #{ subscriptions := Subs }=Data) ->
    {Job, Data2} = dequeue(Data),
    Data3 = start(Job, Data2),
    Info = status_info(Data3),
    State = state(Data3),
    notify(Info, Subs),
    {next_state, State, Data3};
    
red({call, From}, status, Data) ->
    Info = status_info(Data),
    State = state(Data),
    {next_state, State, Data, [{reply, From, {ok, Info}}]};

red({call, From}, clear, #{ subscriptions := Subs}=Data) ->
    Data2 = clear(Data),
    Info = status_info(Data2),
    State = state(Data2),
    notify(Info, Subs),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

red({call, From}, {subscribe, Topic, Id}, Data) ->
    Data2 = subscribe(Topic, Id, Data),
    Info = status_info(Data2),
    State = state(Data2),
    {next_state, State, Data2, [{reply, From, {ok, Info}}]};

red({call, From}, {schedule, _Job}, Data) ->
    {keep_state, Data, {reply, From, {error, full}}}.

terminate(Reason, _, #{ name := Name}) ->
    cmkit:log({cmqueue, Name, terminate, Reason}),
    ok.

status_info(#{ queue := Q, cap := Cap, status := Status}=Data) ->
    S = maps:merge(Cap, Status),
    State = state(Data),
    S#{ state => State, 
        items =>  #{ pending => [ maps:without([spec], J) || J  <- queue:to_list(Q) ],
                     active => []
                   }}.

clear(#{ status := Status}=Data) ->
    Data#{ queue => queue:new(), status => Status#{ pending => 0, failed => 0 }}.

subscribe(Topic, Id, #{ subscriptions := Subs }=Data) ->
    Data#{ subscriptions => Subs#{ Id => Topic }}.


notify(Info, Subs) ->
    NotesSent = maps:fold( fun(Id, Topic, Sent) ->
                cmsession:tell(Id, #{ Topic => Info }),
                Sent + 1
               end, 0, Subs),
    cmkit:log({cmqueue, notify, NotesSent}).

start(#{ id := _,
         timestamp := _,
         type := _,
         name := _,
         spec := {M, F, Args} } = Job, #{ name := Name }=Data) ->
    
    case apply(M, F, Args) of 
        ok -> 
            cmkit:log({cmqueue, Name, started, Job}),
            Data;
        {error, E} ->
            gen_statem:cast(self(), {failed, Job, E}),
            Data
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

failed(Job, Reason, #{ name := Name, 
                       status := #{ failed := F}=Status}=Data) ->
    cmkit:danger({cmqueue, Name, failed, Job, Reason}),
    Data#{ status => Status#{ failed => F + 1}}.

state(#{ cap := #{ concurrency := C,
                   max := M }, 
         status := #{ pending := P, 
                      active := A }}) -> 
    case C > A of 
        true -> green;
        false -> 
            case P of 
                0 -> green;
                _  ->
                    case M > P of 
                        true -> yellow;
                        false -> red
                    end
            end
    end.
