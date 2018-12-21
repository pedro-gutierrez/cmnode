-module(cmbus).
-export([create/1, delete/1, sub/1, sub/2, unsub/1, unsub/2, pub/2, topics/0]).

create(T) ->
    ok = delete(T),
    pg2:create(T).

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
            case lists:is_member(Pid, Members) of 
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


pub(T, Msg) ->
    spawn(fun() ->
                  case pg2:get_members(T) of
                      Pids when is_list(Pids) ->
                          lists:foreach(fun(Pid) ->
                                                cmcore:update(Pid, Msg)
                                        end, Pids);
                      {error, E} ->
                          cmkit:warning({cmtopic, T, pub, Msg, E})
                  end
          end),
    ok.