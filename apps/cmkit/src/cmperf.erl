-module(cmperf).
-export([bench/2, stats/0, procs/1, procs/0, gc/0]).

bench(Fun, Limit) ->
    {Time, _ } = timer:tc(fun() -> 
                                  [ Fun(I) || I <- lists:seq(1, Limit)]
                          end),
    #{ total_us => Time,
       us_per_op => trunc(Limit/Time),
       ops_per_sec => trunc(Limit*1000000/Time) }.

stats() ->
    CpuInfo = case cpu_sup:util([per_cpu]) of
                  {all, 0, 0, []} -> <<"Not available">>;
                  [_|_] = UtilDescs ->
                      lists:map(fun({Cpus, Busy, NonBusy, _}) ->
                                        #{ num => Cpus, busy => Busy, idle => NonBusy }
                                end, UtilDescs)
              end,


    AvgCpu = trunc(lists:foldl(fun(#{ busy := Busy }, Sum) ->
                                       Busy + Sum
                               end, 0, CpuInfo)/length(CpuInfo)),


    {TotalMem0, _, _} = memsup:get_memory_data(),
    TotalMem = trunc(TotalMem0/(1 bsl 20)),
    UsedMem = trunc(erlang:memory(total) / (1 bsl 20)),





    #{mem => #{ 
                used => UsedMem,
                total => TotalMem
              }, 
      cpu => #{
               average => AvgCpu
              }
     }.

gc() ->
    [ erlang:garbage_collect(Pid) || #{ pid := Pid} <- procs() ].


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


