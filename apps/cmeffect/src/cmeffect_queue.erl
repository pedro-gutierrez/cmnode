-module(cmeffect_queue).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> queue.

effect_apply(#{ query := queues }, SessionId) ->
    Res = [ with_status(Q) || Q <- cmconfig:queues() ],
    cmcore:update(SessionId, #{ queues => Res});

effect_apply(#{ query := queue,
                queue := Name }, SessionId) ->
    Res = case cmconfig:queue(Name) of 
              {ok, Spec} ->
                  with_status(Spec);
              Other -> Other
          end,

    cmcore:update(SessionId, #{ queue => Res});

effect_apply(#{ context := Context,
                queue := Name,
                topic := Topic }, SessionId) ->

    Res = case cmqueue:subscribe(Name, Topic, SessionId) of 
              {ok, QueueInfo} ->
                  #{ status => ok,
                     Topic => QueueInfo };
              {error, E} ->
                  #{ status => error,
                     queue => Name,
                     reason => E }
          end,

    cmcore:update(SessionId, Res#{ context => Context });

effect_apply(#{ context := Context,
                queue := Name,
                info := I,
                task := Task,
                params := P}, SessionId) ->

    Id = cmkit:micros(),
    Params = P#{ id => Id }, 
    Res = case cmqueue:schedule(Name, #{ id => Id, 
                                         timestamp => cmkit:now(),
                                         info => I,
                                         spec => #{ start => {cmtask, schedule, [Task, Params]}} }) of 
              {ok, _} ->
                  #{ status => ok };
              {error, E} ->
                  #{ status => error,
                     queue => Name,
                     reason => E }
          end,

    cmcore:update(SessionId, Res#{ context => Context });


effect_apply(#{ context := Context,
                queue := Name,
                cancel := Job }, SessionId) ->

    Res = case cmqueue:cancel(Name, Job) of 
              {ok, _} ->
                  #{ status => ok };
              {error, E} ->
                  #{ status => error,
                     queue => Name,
                     job => Job,
                     reason => E }
          end,

    cmcore:update(SessionId, Res#{ context => Context });

effect_apply(#{ context := Context,
                queue := Name,
                finish := Job }, SessionId) ->

    Res = case cmqueue:finish(Name, Job) of 
              ok ->
                  #{ status => ok };
              {error, E} ->
                  #{ status => error,
                     queue => Name,
                     job => Job,
                     reason => E }
          end,

    cmcore:update(SessionId, Res#{ context => Context }).

with_status(#{ worker := Name }=Spec) ->
    case cmqueue:status(Name) of 
        {ok, Status } -> 
            Spec#{ status => Status };
        {error, E} ->
            Spec#{ error => E }
    end.

