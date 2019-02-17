-module(cmbus).
-export([create/1, 
         create_sub/1,
         create_sub/2,
         delete/1,
         members/1,
         peers/1,
         closest/1,
         sub/1, 
         sub/2, 
         unsub/1, 
         unsub/2, 
         pub/2, 
         topics/0]).

create(T) ->
    pg2:create(T).

create_sub(T) ->
    create_sub(T,self()).

create_sub(T, Pid) ->
    pg2:create(T),
    pg2:join(T, Pid).

delete(T) ->
    pg2:delete(T).

topics() ->
    pg2:which_groups().

sub(T) ->
    sub(T, self()).

sub(T, Pid) ->
    case pg2:get_members(T) of 
        {error, {no_such_group, T}} ->
            {error, no_such_group};
        [] ->
            pg2:join(T, Pid);
        Members when is_list(Members) ->
            case lists:member(Pid, Members) of 
                false ->
                    pg2:join(T, Pid);
                true ->
                    ok
            end
    end.

unsub(T) ->
    unsub(T, self()).

unsub(T, Pid) ->
    pg2:leave(T, Pid).

closest(T) ->
    pg2:get_closest_pid(T).

members(T) -> 
    case pg2:get_members(T) of 
        {error, _} -> [];
        Pids -> Pids
    end.

peers(T) ->
    [Pid || Pid <- members(T), Pid =/= self() ].

pub(T, Msg) ->
    spawn(fun() ->
                  lists:foreach(fun(Pid) ->
                                        cmcore:update(Pid, Msg)
                                end, members(T))
          end),
    ok.
