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

green(cast, finished, #{ status := #{ active := A}=Status}=Data) -> 
    Data2 = Data#{ status => Status#{ active => A - 1}},
    {next_state, state(Data2), Data2};

green({call, From}, status, Data) ->
    reply_status(From, Data);

green({call, From}, clear, Data) ->
    Data2 = clear(Data),
    reply_status(From, Data2);

green({call, From}, {schedule, Job}, #{ status := #{ active := A },
                                        cap := #{ concurrency := A }}=Data) ->
    
    Data2 = enqueue(Job, Data),
    reply_status(From, Data2);

green({call, From}, {schedule, Job}, #{ status := #{ active := A }=Status}=Data) ->
    Data2 = Data#{ status => Status#{ active => A + 1}},
    Data3 = start(Job, Data2),
    reply_status(From, Data3).

yellow(info, _Msg, Data) ->
    {next_state, state(Data), Data};

yellow(cast, {failed, Job, Reason}, Data) ->
    Data2 = failed(Job, Reason, Data),
    {Job, Data3} = dequeue(Data2),
    Data4 = start(Job, Data3),
    {next_state, state(Data4), Data4};

yellow(cast, finished, Data) ->
    {Job, Data2} = dequeue(Data),
    Data3 = start(Job, Data2),
    {next_state, state(Data3), Data3};

yellow({call, From}, status, Data) ->
    reply_status(From, Data);

yellow({call, From}, clear, Data) ->
    Data2 = clear(Data),
    reply_status(From, Data2);

yellow({call, From}, {schedule, Job}, Data) ->
    Data2 = enqueue(Job, Data),
    reply_status(From, Data2).

red(info, _Msg, Data) ->
    {next_state, state(Data), Data};

red(cast, {failed, Job, Reason}, Data) ->
    Data2 = failed(Job, Reason, Data),
    {Job, Data3} = dequeue(Data2),
    Data4 = start(Job, Data3),
    {next_state, state(Data4), Data4};

red(cast, finished, Data) ->
    {Job, Data2} = dequeue(Data),
    Data3 = start(Job, Data2),
    {next_state, state(Data3), Data3};

red({call, From}, status, Data) ->
    reply_status(From, Data);

red({call, From}, clear, Data) ->
    Data2 = clear(Data),
    reply_status(From, Data2);

red({call, From}, {schedule, _Job}, Data) ->
    {keep_state, Data, {reply, From, {error, full}}}.

terminate(Reason, _, #{ name := Name}) ->
    cmkit:log({cmqueue, Name, terminate, Reason}),
    ok.

reply_status(From, #{ cap := Cap, status := Status}=Data) ->
    S = maps:merge(Cap, Status),
    State = state(Data),
    Reply = {ok, S#{ state => State }},
    {next_state, State, Data, [{reply, From, Reply}]}.

clear(#{ status := Status}=Data) ->
    Data#{ queue => queue:new(), status => Status#{ pending => 0, failed => 0 }}.


start({M, F, Args}=Job, #{ name := Name }=Data) ->
    
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
