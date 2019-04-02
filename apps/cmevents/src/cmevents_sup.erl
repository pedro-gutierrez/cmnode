-module(cmevents_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Stores) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Stores]).

init([Stores]) ->
    Writers = [cmkit:child_spec(cmevents_util:store_id(S),
                                cmevents_writer,
                                [S],
                                permanent,
                                worker) || S <- Stores],
    {ok, { {one_for_one, 0, 1}, Writers}}.
