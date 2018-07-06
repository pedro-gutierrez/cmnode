-module(cmtask_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, new_task/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = cmkit:child_spec(cmtask_worker,
                            cmtask_worker,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

new_task(Name, Params) ->
    supervisor:start_child(?MODULE, [Name, Params]).

