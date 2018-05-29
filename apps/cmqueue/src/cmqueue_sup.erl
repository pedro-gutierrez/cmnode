-module(cmqueue_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 0, 1}, [ queue_spec(Q) || Q <- cmconfig:queues() ]  }}.

queue_spec(#{name := Name}=Spec) ->
    cmkit:child_spec(Name, cmqueue_worker, [Spec], worker).
