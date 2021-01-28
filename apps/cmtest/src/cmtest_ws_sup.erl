-module(cmtest_ws_sup).
-behaviour(supervisor).
-export([start_link/0, new/3, stop/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Spec = cmkit:child_spec(cmwsc,
                            cmwsc,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

new(Name, Config, Owner) ->
    supervisor:start_child(?MODULE, [Name, Config, Owner]).

stop(Pid) ->
    cmwsc:stop(Pid).
