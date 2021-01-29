-module(cmkube).
-export([ do/1, await/1 ]).
-define(RETRIES, 120).

do(#{ name := Name,
      namespace := Ns,
      resource := service,
      server := #{ api := Url,
                   token := Token },
      state := absent }) -> 

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    case kuberl_core_v1_api:delete_namespaced_service(Ctx, Name, Ns, Opts) of 
        {ok, Data, _} ->
            {ok, Data};
        {error, Reason, #{ status := 404 }} ->
            {ok, Reason};
        {error, E, _} ->
            {error, E}
    end;

do(#{ name := Name,
      namespace := Ns,
      resource := service,
      server := #{ api := Url,
                   token := Token },
      state := present,
      props := #{ labels := Labels,
                  spec := #{ type := Type,
                             ports := Ports }}} = Spec) ->

    case do(Spec#{ state => absent }) of 
        {ok, _} -> 
            Service = #{ apiVersion => <<"v1">>,
                         kind => <<"Service">>,
                         metadata => #{ name => Name,
                                        namespace => Ns,
                                        labels => Labels },
                         spec => #{ type => Type,
                                    selector => Labels,
                                    ports => Ports } },


            Ctx = ctx:background(),
            Opts = opts(Url, Token),
            case kuberl_core_v1_api:create_namespaced_service(Ctx, Ns, Service, Opts) of 
                {ok, Data, _} ->
                    {ok, Data};
                {error, E, _} ->
                    {error, E}
            end;
        Other ->
            Other
    end;


do(#{ name := Name,
      namespace := Ns,
      resource := deployment,
      server := #{ api := Url,
                   token := Token },
      state := scaled,
      replicas := Replicas }) ->

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    cmkit:log({cmkube, deployment, Name, scaling, Replicas}),

    Scale = [#{ op => replace, path => <<"/spec/replicas">>, value => Replicas}],
    case kuberl_apps_v1_api:patch_namespaced_deployment(Ctx, Name, Ns, Scale, Opts) of 
        {ok, _, _} ->
            ok;
        {error, _, #{ status := 404 }} when Replicas =:= 0 -> 
            ok;
        {error, E, _} ->
            {error, E}
    end;

do(#{ name := Name,
      namespace := Ns,
      resource := deployment,
      server := #{ api := Url,
                   token := Token },
      state := absent,
      props := Props } = Params) -> 
    case do(Params#{ state => scaled, replicas => 0 }) of 
        ok ->
            case await(#{ namespace => Ns,
                          resource => pod,
                          server => #{ api => Url,
                                       token => Token },
                          state => <<"Running">>,
                          props => Props,
                          retries => ?RETRIES,
                          sleep => 1000,
                          exact => 0 }) of 
                ok ->
                    Ctx = ctx:background(),
                    Opts = opts(Url, Token),
                    cmkit:log({cmkube, deployment, Name, deleting}),
                    case kuberl_apps_v1_api:delete_namespaced_deployment(Ctx, Name, Ns, #{}, Opts) of 
                        {error, Reason, #{ status := 404 }} ->
                            {ok, Reason};
                        {error, E, _} -> 
                            {error, E};
                        {ok, Data, _ } -> 
                            cmkit:log({cmkube, deployment, Name, deleted, Data}),
                            {ok, Data}
                    end;
                {error, E} ->
                    {error, E}
            end;
        Other ->
            Other
    end;

do(#{ name := Name,
      namespace := Ns,
      resource := deployment,
      server := #{ api := Url,
                   token := Token },
      state := present,
      props := #{ labels := Labels,
                  replicas := Replicas,
                  spec := Spec } = Props} = Params) ->

    case do(Params#{ state => absent }) of 
        {ok, _ } ->
            Dep = #{ apiVersion => <<"apps/v1">>,
                     kind => <<"Deployment">>,
                     metadata => #{ name => Name,
                                    namespace => Ns,
                                    labels => Labels },
                     spec => #{ replicas => Replicas,
                                selector => #{ matchLabels => Labels },
                                template =>
                                    #{ metadata => #{ labels => Labels },
                                       spec => Spec } }},

            Ctx = ctx:background(),
            Opts = opts(Url, Token),
            case kuberl_apps_v1_api:create_namespaced_deployment(Ctx, Ns, Dep, Opts) of 
                {error, E, _} -> {error, E};
                {ok, Data, _} -> 
                    case await(#{ namespace => Ns,
                                  resource => pod,
                                  server => #{ api => Url,
                                               token => Token },
                                  state => <<"Running">>,
                                  props => Props,
                                  retries => ?RETRIES,
                                  sleep => 1000,
                                  exact => Replicas }) of 
                        ok -> 
                            {ok, Data};
                        Other ->
                            Other
                    end
            end;
        Other ->
            Other
    end;

do(#{ name := Name,
      namespace := Ns,
      resource := statefulset,
      server := #{ api := Url,
                   token := Token },
      state := scaled,
      replicas := Replicas }) -> 

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    cmkit:log({cmkube, statefulset, Name, scaling, Replicas}),

    Scale = [#{ op => replace, path => <<"/spec/replicas">>, value => Replicas}],
    case kuberl_apps_v1_api:patch_namespaced_stateful_set(Ctx, Name, Ns, Scale, Opts) of 
        {ok, _, _} ->
            ok;
        {error, _, #{ status := 404 }} when Replicas =:= 0 -> 
            ok;
        {error, E, _} ->
            {error, E}
    end;

do(#{ name := Name,
      namespace := Ns,
      resource := statefulset,
      server := #{ api := Url,
                   token := Token },
      state := absent,
      props := Props } = Params) -> 

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    case do(Params#{ state => scaled, replicas => 0 }) of 
        ok ->
            case await(#{ namespace => Ns,
                          resource => pod,
                          server => #{ api => Url,
                                       token => Token },
                          state => <<"Running">>,
                          props => Props,
                          retries => ?RETRIES,
                          sleep => 1000,
                          exact => 0 }) of 
                ok ->
                    cmkit:log({cmkube, statefulset, Name, deleting}),
                    case kuberl_apps_v1_api:delete_namespaced_stateful_set(Ctx, Name, Ns, #{}, Opts) of 
                        {error, Reason, #{ status := 404 }} ->
                            {ok, Reason};
                        {error, E, _} -> 
                            {error, E};
                        {ok, Data, _ } -> 
                            cmkit:log({cmkube, statefulset, Name, deleted}),
                            {ok, Data}
                    end;

                {error, E} ->
                    {error, E}
            end;
        Other ->
            Other
    end;

do(#{ name := Name,
      namespace := Ns,
      resource := statefulset,
      server := #{ api := Url,
                   token := Token },
      state := present,
      props := #{ labels := Labels,
                  replicas := Replicas,
                  serviceName := ServiceName,
                  spec := Spec } = Props} = Params) ->

    case do(Params#{ state => absent }) of 
        {ok, _ } ->
            Dep = #{ apiVersion => <<"apps/v1">>,
                     kind => <<"StatefulSet">>,
                     metadata => #{ name => Name,
                                    namespace => Ns,
                                    labels => Labels },
                     spec => #{ replicas => Replicas,
                                serviceName => ServiceName,
                                selector => #{ matchLabels => Labels },
                                template =>
                                    #{ metadata => #{ labels => Labels },
                                       spec => Spec } }},

            Ctx = ctx:background(),
            Opts = opts(Url, Token),
            case kuberl_apps_v1_api:create_namespaced_stateful_set(Ctx, Ns, Dep, Opts) of 
                {error, E, _} -> {error, E};
                {ok, Data, _} -> 
                    case await(#{ namespace => Ns,
                                  resource => pod,
                                  server => #{ api => Url,
                                               token => Token },
                                  state => <<"Running">>,
                                  props => Props,
                                  retries => ?RETRIES,
                                  sleep => 1000,
                                  exact => Replicas }) of 
                        ok -> 
                            {ok, Data};
                        Other ->
                            Other
                    end
            end;
        Other ->
            Other
    end;


do(#{ name := _,
      namespace := Ns,
      resource := statefulset,
      server := #{ api := Url,
                   token := Token },
      state := upgraded,
      props := Props } = Params) ->

    case do(Params#{ state => scaled, replicas => 0 }) of 
        ok ->
            case await(#{ namespace => Ns,
                          resource => pod,
                          server => #{ api => Url,
                                       token => Token },
                          state => <<"Running">>,
                          props => Props,
                          retries => ?RETRIES,
                          sleep => 1000,
                          exact => 0 }) of 
                ok -> 
                    do(Params#{ state => patched })
            end;
        Other ->
            Other
    end;



do(#{ name := Name,
      namespace := Ns,
      resource := statefulset,
      server := #{ api := Url,
                   token := Token },
      state := patched,
      props := #{ labels := Labels,
                  replicas := Replicas,
                  serviceName := ServiceName,
                  spec := Spec } = Props} = _Params) ->

    Dep = #{ apiVersion => <<"apps/v1">>,
             kind => <<"StatefulSet">>,
             metadata => #{ name => Name,
                            namespace => Ns,
                            labels => Labels },
             spec => #{ replicas => Replicas,
                        serviceName => ServiceName,
                        selector => #{ matchLabels => Labels },
                        template =>
                            #{ metadata => #{ labels => Labels#{ date => cmkit:to_bin(cmkit:now()) } },
                               spec => Spec } }},

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    case kuberl_apps_v1_api:patch_namespaced_stateful_set(Ctx, Ns, Dep, Opts) of 
        {error, E, _} -> {error, E};
        {ok, Data, _} -> 
            case await(#{ namespace => Ns,
                          resource => pod,
                          server => #{ api => Url,
                                       token => Token },
                          state => <<"Running">>,
                          props => Props,
                          retries => ?RETRIES,
                          sleep => 1000,
                          exact => Replicas }) of 
                ok -> 
                    {ok, Data};
                Other ->
                    Other
            end
    end;

do(#{ namespace := Ns,
      resource := pod,
      server := #{ api := Url,
                   token := Token },
      props := #{ labels := Labels }}) ->

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    Opts2 =  Opts#{ params => #{ labelSelector => to_query_params(Labels)}},
    case kuberl_core_v1_api:list_namespaced_pod(Ctx, Ns, Opts2) of 
        {ok, #{ items := Items }, _} -> 
            {ok, Items};
        Other -> 
            Other
    end;

do(#{ namespace := Ns,
      resource := pod,
      server := #{ api := Url,
                   token := Token },
      state := absent } = Params) ->

    case do(maps:without([state], Params)) of
        {ok, Items} ->
            Names = [ Name || #{ metadata := #{ name := Name }} <- Items ],
            cmkit:log({cmkube, deleting, pods, Names}),
            Ctx = ctx:background(),
            Opts = opts(Url, Token),
            case delete_pods(Names, Ns, Ctx, Opts) of 
                ok -> {ok, length(Names)};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;


do(#{ namespace := Ns,
      name := Name,
      resource := job,
      namespace := Ns,
      server := #{ api := Url,
                   token := Token },
      state := absent }) ->

    Ctx = ctx:background(),
    Opts = opts(Url, Token),

    case kuberl_batch_v1_api:delete_namespaced_job(Ctx, Name, Ns, #{}, Opts) of
        {error, E, _} -> {error, E};
        {ok, _, _} -> ok
    end;

do(#{ namespace := Ns,
      name := Name,
      resource := job,
      namespace := Ns,
      server := #{ api := Url,
                   token := Token },
      state := present,
      props := #{ labels := Labels,
                  spec := Spec} = Props} = Params) ->

    Yaml = #{ apiVersion => <<"batch/v1">>,
              kind => <<"Job">>,
              metadata => #{ name => Name,
                             namespace => Ns,
                             labels => Labels },
              spec => #{ template => #{ metadata => #{ labels => Labels },
                                        spec => Spec } }},

    cmkit:log({cmkube, Yaml}),
    Ctx = ctx:background(),
    Opts = opts(Url, Token),

    do(Params#{ state => absent }), 
    case await(#{ namespace => Ns,
                  resource => job,
                  server => #{ api => Url,
                               token => Token },
                  props => Props,
                  retries => ?RETRIES,
                  sleep => 1000,
                  exact => 0 }) of 
        ok ->
            case kuberl_batch_v1_api:create_namespaced_job(Ctx, Ns, Yaml, Opts) of 
                {error, E, _} -> {error, E};
                {ok, _, _} -> ok
            end;
        {error, E} ->
            {error, E}
    end; 

do(#{ namespace := Ns,
      resource := job,
      server := #{ api := Url,
                   token := Token },
      props := #{ labels := Labels }}) ->

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    Opts2 =  Opts#{ params => #{ labelSelector => to_query_params(Labels)}},
    case kuberl_batch_v1_api:list_namespaced_job(Ctx, Ns, Opts2) of 
        {ok, #{ items := Items }, _} -> 
            {ok, Items};
        Other -> 
            Other
    end;

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"create">>,
      resource := <<"configmap">>,
      name := Name,
      data := Data
    }) ->

    Ctx = ctx:background(),
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    kuberl_core_v1_api:delete_namespaced_config_map(Ctx, Name, Ns, #{}, #{ cfg => Cfg }),
    kuberl_core_v1_api:create_namespaced_config_map(Ctx, Ns, #{ apiVersion => <<"v1">>,
                                                                kind => <<"ConfigMap">>,
                                                                metadata => #{ name => Name,
                                                                               namespace => Ns },
                                                                data => Data }, #{ cfg => Cfg });




do(#{ namespace := Ns,
      name := Name,
      resource := secret,
      server := #{ api := Url,
                   token := Token },
      state := absent }) ->

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    case kuberl_core_v1_api:delete_namespaced_secret(Ctx, Name, Ns, #{}, Opts) of 
        {error, Reason, #{ status := 404 }} ->
            {ok, Reason};
        {error, E, _} -> {error, E};
        {ok, Status, _} -> 
            {ok, Status}
    end;

do(#{ namespace := Ns,
      name := Name,
      resource := secret,
      server := #{ api := Url,
                   token := Token },
      state := present,
      props := #{ key := Key,
                  cert := Cert }}=Params) ->

    case do(Params#{ state => absent }) of 
        {ok, _} ->
            Ctx = ctx:background(),
            Opts = opts(Url, Token),
            Secret = #{ apiVersion => <<"v1">>,
                        kind => <<"Secret">>,
                        metadata => #{ name => Name,
                                       namespace => Ns },
                        type => <<"kubernetes.io/tls">>,
                        stringData => #{ <<"tls.crt">> => Cert,
                                         <<"tls.key">> => Key }},
            case kuberl_core_v1_api:create_namespaced_secret(Ctx, Ns, Secret, Opts) of 
                {error, #{ code := Code,
                           message := Message }, _} -> {error, #{ code => Code, 
                                                                  reason => Message}};
                {error, E, _} -> {error, E};
                {ok, Status, _} -> 
                    {ok, Status}
            end;
        Other ->
            Other
    end;

do(Q) ->
    {error, #{ status => not_supported_yet,
               query => Q}}.

await(#{ retries := Retries,
         sleep := Millis, 
         exact := Exact }=Params) ->

    FilterFun = case maps:get(state, Params, undef) of 
                    undef -> 
                        fun(_) -> true end;
                    State ->
                        fun(#{ status := #{ phase := Phase }}) ->
                                Phase =:= State
                        end
                end,

    case Retries of 
        0 -> 
            {error, timeout};
        _ -> 
            timer:sleep(Millis),
            case do(Params) of 
                {ok, Items} ->
                    case length(lists:filter(FilterFun, Items)) of 
                        Exact -> 
                            ok;
                        Other -> 
                            cmkit:log({cmkube, await, Params, Other, Exact, sleeping}),
                            await(Params#{ retries => Retries -1 })
                    end;
                _ -> 
                    await(Params#{ retries => Retries -1 })
            end
    end.

opts(Host, Token) ->    
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    #{ cfg => Cfg }.


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
