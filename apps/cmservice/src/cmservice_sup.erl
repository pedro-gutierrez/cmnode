-module(cmservice_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, new/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = cmkit:child_spec(cmservice_worker,
                            cmservice_worker,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

new(App, Params) ->
    supervisor:start_child(?MODULE, [App, Params]).
