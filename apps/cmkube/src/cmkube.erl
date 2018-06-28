-module(cmkube).
-export([
         do/1,
         await/1
        ]).

do(#{ host := Host,
        token := Token,
        verb := get,
        resource := api_versions }) ->
    do(kuberl_core_api, get_api_versions, Host, Token);


do(#{ host := Host,
        token := Token,
        verb := list,
        resource := namespaces}) ->
    do(kuberl_core_v1_api, list_namespace, Host, Token);

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      service := App,
      verb := <<"list">>,
      resource := <<"endpoints">> }) ->
    do(kuberl_core_v1_api, list_namespaced_endpoints, Ns, 
       #{ labelSelector => <<"app=", App/binary>> }, Host, Token);

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
      verb := <<"create">>,
      resource := <<"service">>,
      name := Name,
      labels := Labels,
      ports := Ports
    }) ->
    
    Ctx = ctx:background(),
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    kuberl_core_v1_api:delete_namespaced_service(Ctx, Name, Ns, #{ cfg => Cfg }),
    kuberl_core_v1_api:create_namespaced_service(Ctx, Ns, #{ apiVersion => <<"v1">>,
                                                             kind => <<"Service">>,
                                                             metadata => #{ name => Name,
                                                                            namespace => Ns,
                                                                            labels => Labels },
                                                             spec => #{ selector => Labels, 
                                                                        ports => Ports } }, #{ cfg => Cfg });

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"list">>,
      resource := <<"pods">>,
      labels := Labels
    }) ->

    Ctx = ctx:background(),
    Opts = opts(Host, Token),
    Opts2 =  Opts#{ params => #{ labelSelector => cmkube_util:to_query_params(Labels)}},
    case kuberl_core_v1_api:list_namespaced_pod(Ctx, Ns, Opts2) of 
        {ok, #{ items := Items }, _} -> 
            {ok, Items};
        Other -> 
            Other
    end;


do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"delete">>,
      resource := <<"deployment">>,
      retries := Retries,
      name := Name }=Params) ->

    Ctx = ctx:background(),
    Opts = opts(Host, Token),
    case kuberl_apps_v1_api:delete_namespaced_deployment(Ctx, Name, Ns, #{}, Opts) of 
        {error, E} -> {error, E};
        _ -> 
            await(Params#{ status => <<"Running">>,
                               retries => Retries,
                               sleep => 1000,
                               exact => 0,
                               resource => <<"pods">> })
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
    
    cmkit:log({cmkube, deployment, Name, deleting}),
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
                                        resource => <<"pods">> }) of 
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

do(Q) ->
    {error, #{ status => not_supported_yet,
               query => Q#{ token => <<"NOT SHOWN">> }}}.

await(#{ status := Status,
         retries := Retries,
         sleep := Millis, 
         exact := Exact }=Params) ->

    case Retries of 
        0 -> 
            {error, timeout};
        _ -> 
            timer:sleep(Millis),
            case do(Params#{ verb => <<"list">> }) of 
                {ok, Items} ->
                    case length(filter_by_status(Status, Items)) of 
                        Exact -> 
                            ok;
                        Other -> 
                            cmkit:log({cmkube, await, Params, Status, Other, Exact, sleeping}),
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


do(Api, Op, Host, Token) ->
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    Api:Op(ctx:background(), #{cfg => Cfg}).

do(Api, Op, Ns, Opts, Host, Token) ->
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    Api:Op(ctx:background(), Ns, #{ params => Opts,
                                    cfg => Cfg }).
