-module(cmperf).
-export([stats/0, procs/1, procs/0]).

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

procs() -> procs(5).

procs(Limit) ->
    lists:sublist(
        lists:sort(fun proc_by_mem/2, lists:map(fun proc_info/1, erlang:processes())), Limit).

proc_info(Pid) -> 
    maps:from_list([{pid, Pid}, 
                    erlang:process_info(Pid, current_function), 
                    erlang:process_info(Pid,memory),
                    proc_name(Pid)
                   ]).

proc_name(Pid) -> 
    case erlang:process_info(Pid, registered_name) of 
        [] -> {registered_name, none};
        Info -> Info
    end.

proc_by_mem(#{ memory := M1 }, #{ memory := M2 }) -> 
    M1 >= M2.


