-module(cmdb_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Buckets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Buckets]).

init([Buckets]) ->


    Writers = [cmkit:child_spec(spec_id(B, writer),
                                cmdb_writer,
                                [B],
                                permanent,
                                worker) || B <- Buckets],

    {ok, { {one_for_one, 0, 1}, Writers}}.


spec_id(#{ name := Name }, Type) ->
    cmkit:to_atom(cmkit:bin_join([ cmkit:to_bin(Name), cmkit:to_bin(Type) ], <<"_">>)).
