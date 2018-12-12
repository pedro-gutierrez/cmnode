-module(cmtopic).
-export([
         inspect/0, 
         reload/0, 
         create/1, 
         delete/1,
         sub/1,
         unsub/1,
         pub/2
        ]).

reload() ->
    reload(cmconfig:topics()).

reload([]) -> ok;
reload([#{ name := T}|Rem]) ->
    Res = create(T),
    cmkit:log({cmtopic, T, Res}),
    reload(Rem).


create(T) ->
    ok = delete(T),
    pg2:create(T).

delete(T) ->
    pg2:delete(T).

inspect() ->
    pg2:which_groups().

sub(T) ->
    %% TODO: monitor this pid
    %% in case it dies, then remove it from the 
    %% group
    pg2:join(T, self()).

unsub(T) ->
    pg2:leave(T, self()).


pub(T, Msg) ->
    spawn(fun() ->
                  case pg2:get_members(T) of 
                      Pids when is_list(Pids) ->
                          lists:foreach(fun(Pid) ->
                                                Pid ! Msg
                                        end, Pids);
                      {error, E} ->
                          cmkit:warning({cmtopic, T, pub, Msg, E})
                  end
          end),
    ok.
