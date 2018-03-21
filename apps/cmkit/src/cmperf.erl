-module(cmperf).
-export([stats/0]).

stats() ->
  CpuInfo = case cpu_sup:util([per_cpu]) of
    {all, 0, 0, []} -> <<"Not available">>;
    [_|_] = UtilDescs ->
                lists:map(fun({Cpus, Busy, NonBusy, _}) ->
                              #{ num => Cpus, busy => Busy, idle => NonBusy }
                          end, UtilDescs)
  end,
  #{mem => maps:from_list(memsup:get_system_memory_data()),
    cpu => CpuInfo
   }.
