-module(cmcore_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    ContextSup = cmkit:child_spec(cmcore_context_sup,
                                  cmcore_context_sup,
                                  [],
                                  permanent,
                                  supervisor),
    
    {ok, { {one_for_one, 0, 1}, [
                                 ContextSup
                                ]}}.

