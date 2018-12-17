-module(cmtopic).
-export([reload/0]).

reload() ->
    reload(cmconfig:topics()).

reload([]) -> ok;
reload([#{ name := T}|Rem]) ->
    Res = cmbus:create(T),
    cmkit:log({cmtopic, T, Res}),
    reload(Rem).
