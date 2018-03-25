-module(cmcore_sup).
-behaviour(supervisor).
-export([start_link/0, start_context/1]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    Spec = cmkit:child_spec(cmcore_context,
                            cmcore_context,
                            [],
                            temporary,
                            worker),
    
    {ok, { {simple_one_for_one, 0, 1}, [Spec]}}.

start_context(Session) ->
    supervisor:start_child(?MODULE, [Session]).
