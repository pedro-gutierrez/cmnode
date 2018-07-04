-module(cmcore_context_sup).
-behaviour(supervisor).
-export([start_link/0, start_context/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    Spec = cmkit:child_spec(cmcore_context,
                            cmcore_context,
                            [],
                            temporary,
                            worker),

    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start_context(Spec, Session) ->
    supervisor:start_child(?MODULE, [Spec, Session]).
