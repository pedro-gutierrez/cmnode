-module(cmkube).
-export([ do/1, await/1 ]).

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
      state := absent,
      props := Props }) -> 

    Ctx = ctx:background(),
    Opts = opts(Url, Token),
    cmkit:log({cmkube, deployment, Name, deleting}),
    case kuberl_apps_v1_api:delete_namespaced_deployment(Ctx, Name, Ns, #{}, Opts) of 
        {error, Reason, #{ status := 404 }} ->
            {ok, Reason};
        {error, E, _} -> {error, E};
        {ok, Data, _ } -> 
            cmkit:log({cmkube, deployment, Name, deleted}),
            case await(#{ namespace => Ns,
                          resource => pod,
                          server => #{ api => Url,
                                       token => Token },
                          state => <<"Running">>,
                          props => Props,
                          retries => 60,
                          sleep => 1000,
                          exact => 0 }) of 
                ok ->
                    {ok, Data};
                {error, E} ->
                    {error, E}
            end
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
                                  retries => 60,
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



do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"delete">>,
      resource := <<"pod">>
    }=Params) ->

    case do(Params#{ verb => <<"list">> }) of
        {ok, Items} ->
            Names = [ Name || #{ metadata := #{ name := Name }} <- Items ],
            cmkit:log({cmkube, deleting, pods, Names}),
            Ctx = ctx:background(),
            Opts = opts(Host, Token),
            delete_pods(Names, Ns, Ctx, Opts);
        Other -> 
            Other
    end;


do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"create">>,
      resource := <<"deployment">>,
      retries := Retries,
      name := Name,
      labels := Labels,
      replicas := Replicas,
      spec := Spec
    }=Params) ->
    
    case do(Params#{ verb => <<"delete">> }) of 
        ok ->
            cmkit:log({cmkube, deployment, Name, creating}),
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
            Opts = opts(Host, Token),
            case kuberl_apps_v1_api:create_namespaced_deployment(Ctx, Ns, Dep, Opts) of 
                {error, #{ message := E }, _} -> {error, E};
                {ok, FullSpec, _} -> 
                    cmkit:log({cmkube, deployment, Name, waiting_for_pods}),
                    case await(Params#{ status => <<"Running">>,
                                        retries => Retries,
                                        sleep => 1000,
                                        exact => Replicas,
                                        resource => <<"pod">> }) of 
                        ok -> 
                            cmkit:log({cmkube, deployment, Name, finished}),
                            {ok, FullSpec};

                        Other ->
                            Other
                    end
            end;
        Other ->
            Other
    end;

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"delete">>,
      resource := <<"secret">>,
      name := Name }) ->
        
    cmkit:log({cmkube, secret, Name, deleting}),
    Ctx = ctx:background(),
    Opts = opts(Host, Token),
    case kuberl_core_v1_api:delete_namespaced_secret(Ctx, Name, Ns, #{}, Opts) of 
        {error, E} -> {error, E};
        _ -> ok
    end;

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"create">>,
      resource := <<"secret">>,
      name := Name,
      key := Key,
      cert := Cert
    }=Params) ->

    case do(Params#{ verb => <<"delete">> }) of 
        ok ->
            cmkit:log({cmkube, secret, Name, creating}),
            Ctx = ctx:background(),
            Opts = opts(Host, Token),
            Spec = #{ apiVersion => <<"v1">>,
                     kind => <<"Secret">>,
                     metadata => #{ name => Name,
                                    namespace => Ns },
                     type => <<"kubernetes.io/tls">>,
                     stringData => #{ <<"tls.crt">> => Cert,
                                      <<"tls.key">> => Key }},
            case kuberl_core_v1_api:create_namespaced_secret(Ctx, Ns, Spec, Opts) of 
                {error, E} -> {error, E};
                {error, #{ code := Code,
                           message := Message }, _} -> {error, #{ code => Code, 
                                                                  reason => Message}};
                Resp -> 
                    cmkit:log({cmkube, secret, Name, create, Resp}),
                    ok
            end;
        Other ->
            Other
    end;



do(Q) ->
    {error, #{ status => not_supported_yet,
               query => Q#{ token => <<"NOT SHOWN">> }}}.

await(#{ state := State,
         retries := Retries,
         sleep := Millis, 
         exact := Exact }=Params) ->

    case Retries of 
        0 -> 
            {error, timeout};
        _ -> 
            timer:sleep(Millis),
            case do(Params) of 
                {ok, Items} ->
                    case length(filter_by_status(State, Items)) of 
                        Exact -> 
                            ok;
                        Other -> 
                            cmkit:log({cmkube, await, Params, State, Other, Exact, sleeping}),
                            await(Params#{ retries => Retries -1 })
                    end;
                _ -> 
                    await(Params#{ retries => Retries -1 })
            end
    end.

filter_by_status(Status, Items) ->
    lists:filter(fun(#{ status := #{ phase := Phase }}) ->
                    Phase =:= Status
                 end, Items).


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
