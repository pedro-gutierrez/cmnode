-module(cmcore_effect_sup).
-behaviour(supervisor).
-export([start_link/0, start_effect/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    Spec = cmkit:child_spec(cmcore_effect,
                            cmcore_effect,
                            [],
                            temporary,
                            worker),
    
    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start_effect(Id) -> 
    supervisor:start_child(?MODULE, [Id]).
