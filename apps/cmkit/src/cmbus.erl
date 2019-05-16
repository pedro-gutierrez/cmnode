-module(cmbus).
-export([create/1, 
         create_sub/1,
         create_sub/2,
         delete/1,
         members/1,
         peers/1,
         closest/1,
         local/1,
         sub/1, 
         sub/2, 
         unsub/1, 
         unsub/2, 
         pub/2, 
         pub/3,
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
    case pg2:get_closest_pid(T) of 
        {error, _} -> 
            {error, no_subscriptions};
        Pid when is_pid(Pid) ->
            {ok, [Pid]}
    end.

local(T) ->
    case pg2:get_local_members(T) of 
        {error, _} -> 
            {error, no_subscriptions};
        Pids when is_list(Pids) ->
            {ok, Pids}
    end.

members(T) -> 
    case pg2:get_members(T) of 
        {error, _} -> 
            {error, no_subscriptions};
        Pids -> 
            {ok, Pids}
    end.

peers(T) ->
    case members(T) of 
        {ok, Pids} ->
            {ok, [Pid || Pid <- Pids, Pid =/= self() ]};
        Other ->
            Other
    end.

pub(T, Msg) ->
    pub(T, Msg, members).

pub(T, Msg, Method) ->
    case subscriptions(T, Method) of 
        {ok, Pids} ->
            lists:foreach(fun(Pid) ->
                                  cmcore:update(Pid, Msg)
                          end, Pids),
            ok;
        Other ->
            Other
    end.

subscriptions(T, members) ->
    members(T);

subscriptions(T, local) ->
    local(T);

subscriptions(T, closest) ->
    closest(T).
