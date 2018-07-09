-module(cmkube_util).
-export([to_query_params/1,
         delete_pods/4]).

to_query_params(Map) -> 
    cmkit:map_join(Map, <<"=">>, <<",">>).


delete_pods([], _, _ , _) -> ok;
delete_pods([N|Rem], Ns, Ctx, Opts) -> 
    case kuberl_core_v1_api:delete_namespaced_pod(Ctx, N, Ns, #{}, Opts) of
        {error, E} -> {error, E};
        {error, #{ code := Code,
                   message := Message }, _} -> {error, #{ code => Code,
                                                          reason => Message}};
        _ ->
            cmkit:log({cmkube, pod, N, Ns, deleted}),
            delete_pods(Rem, Ns, Ctx, Opts)
    end.
