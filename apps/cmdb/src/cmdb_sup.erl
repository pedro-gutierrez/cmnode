-module(cmdb_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    WriterSup = cmkit:child_spec(cmdb_writer_sup,
                                 cmdb_writer_sup,
                                 [],
                                 permanent,
                                 supervisor),
    
    CompilerSup = cmkit:child_spec(cmdb_compiler_sup,
                                   cmdb_compiler_sup,
                                   [],
                                   permanent,
                                   supervisor),

    {ok, { {one_for_one, 0, 1}, [WriterSup, CompilerSup]} }.
