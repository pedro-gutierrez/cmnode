-module(cmtopic_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Specs = lists:map(fun child_spec/1, cmconfig:topics()),
    {ok, { {one_for_one, 0, 1}, Specs}}.

child_spec(#{ name := Name }=T) ->
    Id = cmkit:to_atom(cmkit:bin_join([ cmkit:to_bin(Name),
                                        cmkit:to_bin(topic) ], <<"_">>)),
    cmkit:child_spec(Id,
                     cmtopic_worker,
                     [Id, T],
                     permanent,
                     worker).
